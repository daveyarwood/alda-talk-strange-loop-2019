(ns demo.meteorology
  (:require [alda.core         :refer :all]
            [clj-http.client   :as    http]
            [clojure.data.json :as    json]
            [clojure.edn       :as    edn]))

;; Data source: https://public.opendatasoft.com/explore/dataset/1000-largest-us-cities-by-population-with-geographic-coordinates/table/
(def cities
  [{:name         "New York"
    :coordinates  [40.71 -74.00]
    :instrument   "percussion"
    :transpose    -36
    :volume-ratio 0.85}
   {:name         "Los Angeles"
    :coordinates  [34.05 -118.24]
    :instrument   "upright-bass"
    :transpose    -36
    :volume-ratio 0.90}
   {:name         "St. Louis"
    :coordinates  [38.63 -90.20]
    :instrument   "tenor-saxophone"
    :transpose    -18
    :volume-ratio 0.80}
   {:name         "Durham"
    :coordinates  [35.99 -78.90]
    :instrument   "vibraphone"
    :volume-ratio 0.90}])

(def fallback-cities
  [{:name "Detroit", :coordinates [32.78, -96.80]}
   {:name "Boston", :coordinates [42.36, -71.06]}
   {:name "Austin", :coordinates [30.27, -97.74]}
   {:name "Indianapolis", :coordinates [39.77, -86.16]}])

(defn hourly-forecast
  "Obtain the hourly forecast for a city from the National Weather Service API.

   Returns a seq of hourly periods, each of which is a map containing
   information like the temperature, wind direction and wind speed.

   Throws an exception if the NWS API happens to return non-200, which I've seen
   happen, so calling code needs to handle that scenario."
  [{:keys [coordinates] :as city}]
  (let [[x y] coordinates]
    (-> (http/get (format "https://api.weather.gov/points/%s,%s" x y))
        :body
        json/read-str
        (get-in ["properties" "forecastHourly"])
        http/get
        :body
        json/read-str
        (get-in ["properties" "periods"]))))

(def forecasts-file
  "/tmp/forecasts.edn")

(def forecasts
  (atom nil))

(defn collect-forecasts
  [city]
  (try
    {:city city, :forecast (hourly-forecast city)}
    (catch Throwable t
      (let [fallback-city (merge city (rand-nth fallback-cities))]
        (println
          (format
            "Failed to retrieve hourly forecast for %s, trying %s instead."
            (:name city) (:name fallback-city)))
        (collect-forecasts fallback-city)))))

(defn collect-forecasts!
  []
  (->> cities
       (map collect-forecasts)
       doall
       pr-str
       (spit forecasts-file)))

(defn load-forecasts!
  []
  (reset! forecasts (edn/read-string (slurp forecasts-file)))
  :ok)

(def wind-directions-top
  ["W" "WNW" "NW" "NNW" "N" "NNE" "NE" "ENE" "E"])

(def wind-directions-bottom
  ["W" "WSW" "SW" "SSW" "S" "SSE" "SE" "ESE" "E"])

(def wind-direction->panning
  (let [partitions (count wind-directions-top)
        step-size  (/ 100.0 (dec partitions))
        values     (iterate (partial + step-size) 0)]
    (merge
      (zipmap wind-directions-top values)
      (zipmap wind-directions-bottom values))))

(defn forecast-note
  "Interprets an hourly forecast period as a note.

   The temperature (F) is used verbatim as the MIDI note number.
   The wind direction affects the panning.
   The wind speed affects the volume and note length."
  [volume-ratio {:strs [temperature windDirection windSpeed]}]
  (let [wind-speed (->> windSpeed (re-find #"\d+") Integer/parseInt)]
    [(panning (or (wind-direction->panning windDirection)
                  (throw (ex-info "Unrecognized wind direction"
                                  {:wind-direction windDirection}))))
     (volume (* volume-ratio
                (+ 35
                   (min 65
                        (* 65 (/ wind-speed 10.0))))))
     (note (midi-note temperature)
           (duration (note-length (max wind-speed 0.5))))]))

(def score
  (atom nil))

(defn generate-score!
  []
  (reset! score
          (for [{:keys [city forecast]} @forecasts
                :let [{:keys [instrument volume-ratio transpose]} city]]
            [(part instrument)
             (transposition (or transpose 0))
             (map (partial forecast-note volume-ratio) forecast)]))
  (spit "/tmp/meteorology.alda" (->str @score))
  :ok)

(comment
  (do
    (collect-forecasts!)
    (load-forecasts!)
    (generate-score!)
    (do
      (stop!)
      (clear-history!))
    :ready)

  (play! @score))

