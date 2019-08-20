(ns demo.meteorology
  (:require [alda.core         :refer :all]
            [clj-http.client   :as    http]
            [clojure.data.json :as    json]
            [clojure.edn       :as    edn]))

;; Data source: https://public.opendatasoft.com/explore/dataset/1000-largest-us-cities-by-population-with-geographic-coordinates/table/
(def cities
  [{:name        "New York"
    :coordinates [40.71 -74.00]
    :instrument  "percussion"
    :transpose   -36
    :volume      85}
   {:name        "Los Angeles"
    :coordinates [34.05 -118.24]
    :instrument  "upright-bass"
    :transpose   -36
    :volume      90}
   {:name        "St. Louis"
    :coordinates [38.63 -90.20]
    :instrument  "tenor-saxophone"
    :transpose   -24
    :volume      80}
   {:name        "Durham"
    :coordinates [35.99 -78.90]
    :instrument  "vibraphone"
    :volume      90}])

(defn hourly-forecast
  "Obtain the hourly forecast for a city from the National Weather Service API.

   Returns a seq of hourly periods, each of which is a map containing
   information like the temperature, wind direction and wind speed."
  [{:keys [coordinates]}]
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

(defn collect-forecasts!
  []
  (->> (for [city cities]
         {:city city, :forecast (hourly-forecast city)})
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
   The wind speed affects the note length."
  [{:strs [temperature windDirection windSpeed]}]
  (let [wind-speed (->> windSpeed (re-find #"\d+") Integer/parseInt)]
    [(panning (or (wind-direction->panning windDirection)
                  (throw (ex-info "Unrecognized wind direction"
                                  {:wind-direction windDirection}))))
     (note (midi-note temperature)
           (duration (note-length (max wind-speed 0.5))))]))

(def score
  (atom nil))

(defn generate-score!
  []
  (reset! score
          (for [{:keys [city forecast]} @forecasts
                :let [{:keys [instrument volume transpose]} city]]
            [(part instrument)
             (vol volume)
             (transposition (or transpose 0))
             (map forecast-note forecast)]))
  :ok)

(comment
  (do
    (collect-forecasts!)
    (load-forecasts!)
    (generate-score!)
    (do
      (stop!)
      (clear-history!)))

  (play! @score))

