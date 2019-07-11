(ns seasonal-chart.core
  (:require [ajax.core :refer [POST]]
            [clojure.data.json :as json]
            [clojure.core.async :as a])
  (:gen-class))

(def q
  "query ($id: Int) {
    Media (id: $id, type: ANIME) {
      id
      title {
        romaji
        english
        native
      }
    }
  }")

(defn query [q vars]
  (let [url "https://graphql.anilist.co"
        ch (a/chan 1)
        ; wait until result is available and pass
        ; it through the channel
        handler (fn [result] (a/>!! ch result))]
      (POST url {:params {:query q,
                          :variables vars},
                 :handler handler
                 :format :json,
                 :response-format :json})
      ; unwrap outer map
      ((a/<!! ch) "data")))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
