(ns seasonal-chart.core
  (:require [ajax.core :refer [POST]]
            [clojure.data.json :as json]
            [clojure.core.async :as a]
            [clojure.string :as str])
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

(defn parsevar [vars]
  (let [varstrs (map (comp
                       #(str/split % #"\.")
                       name)
                     (keys vars))
        to-params (fn [[vartype varname]]
                    (str "$" varname ": " vartype))
        params    (str/join ", " (map to-params varstrs))
        varmap (zipmap (map second varstrs) (vals vars))]
    [params varmap]))

(defn query [q vars]
  (let [url "https://graphql.anilist.co"
        [params varmap] (parsevar vars)
        wrappedq (str "query(" params "){\n" q "\n}")
        ch (a/chan 1)
        ; wait until result is available and pass
        ; it through the channel
        handler (fn [result] (a/>!! ch (result "data")))
        error-handler (fn [er]
                        (println er)
                        (a/>!! ch :error))]
      (POST url {:params {:query wrappedq,
                          :variables varmap},
                 :handler handler
                 :error-handler error-handler
                 :format :json,
                 :response-format :json})
      ; unwrap outer map
      (a/<!! ch)))

(defn get-season-page [year season pagesize pagenum]
  (let [q "Page(page: $pagenum perPage: $pagesize){
             pageInfo{
               hasNextPage
             }
             media(season: $season seasonYear: $year){
               title{
                 userPreferred
               }
             }
           }"
        vars {:Int.year year
              :MediaSeason.season season
              :Int.pagesize pagesize
              :Int.pagenum pagenum}]
    (query q vars)))

(defn get-season
  ([year season] (get-season year season 1))
  ([year season page]
   (let [this-page (get-season-page year season 50 page)
         has-next (((this-page "Page") "pageInfo") "hasNext")
         shows ((this-page "Page") "media")]
     (if has-next
       (concat shows
               (get-season-page year season 50 (inc page)))
       shows))))
(def a (get-season 2019 :SPRING))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
