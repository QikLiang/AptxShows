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

(defn get-page [q vars pagesize pagenum]
  (let [new-q (format
                "Page(page: $pagenum perPage: $pagesize){
                  pageInfo{
                    hasNextPage
                  }
                  %s
                }" q)
        page-vars {:Int.pagesize pagesize
                   :Int.pagenum pagenum}
        new-vars (merge page-vars vars)]
    (query new-q new-vars)))

(defn get-all-pages
  ([q vars] (get-all-pages q vars 1))
  ([q vars page]
   (let [this-page (get-page q (dissoc vars :page-type)
                             50 page)
         has-next  (((this-page "Page") "pageInfo") "hasNext")
         results   ((this-page "Page") (vars :page-type))]
     (if has-next
       (concat results
               (get-all-pages q vars 50 (inc page)))
       results))))

(defn get-season [year season]
  (let [q "media(season: $season seasonYear: $year){
             title{
               userPreferred
             }
           }"
        vars {:page-type "media"
              :Int.year year
              :MediaSeason.season season}]
    (get-all-pages q vars)))
(def a (get-season 2019 :SPRING))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
