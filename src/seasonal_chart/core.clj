(ns seasonal-chart.core
  (:require [ajax.core :refer [POST]]
            [clojure.data.json :as json]
            [clojure.core.async :as a]
            [clojure.string :as str])
  (:gen-class))

(defn parsevar
  "Format parameter variables for GraphQL query.
  Expects input in the form {:Type.name value, ...}
  and outputs a param map {'name' value, ...} and
  a string '$name: Type, ...'."
  [vars]
  (let [varstrs (map (comp
                       #(str/split % #"\.")
                       name)
                     (keys vars))
        to-params (fn [[vartype varname]]
                    (str "$" varname ": " vartype))
        params (str/join ", " (map to-params varstrs))
        varmap (zipmap (map second varstrs) (vals vars))]
    [params varmap]))

(defn query
  "Send AJAX request to Anilist GraphQL API.
  Expects parameters to be handled by parsevar."
  [q vars]
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

(defn get-page
  "Sends a single Page query."
  [q vars pagesize pagenum]
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
  "Makes multiple Page queries and concats the results.
  Require vars to contain :page-type which unwraps the
  content of the page."
  ([q vars] (get-all-pages q vars 1))
  ([q vars page]
   (let [this-page (get-page q (dissoc vars :page-type)
                             50 page)
         has-next  (get-in this-page ["Page"
                                      "pageInfo"
                                      "hasNext"])
         results   ((this-page "Page") (vars :page-type))]
     (if has-next
       (concat results
               (get-all-pages q vars 50 (inc page)))
       results))))

(defn get-season
  "Return all airing shows for a season."
  [year season]
  (let [q "media(season: $season seasonYear: $year
                 type: ANIME){
            id
            idMal
            title{
              romaji
              english
            }
            coverImage { medium }
            format
            staff {
              edges {
                role
                node{
                  id
                  name {
                    first
                    last
                    native
                  }
                }
              }
            }
          }"
        vars {:page-type "media"
              :Int.year year
              :MediaSeason.season season}]
    (get-all-pages q vars)))

(defn get-user-data
  "Fetch all user data needed to personalize results."
  [user]
 (let [q "MediaListCollection(userName: $user
                              type: ANIME
                              status: COMPLETED){
           lists {
             entries {
               score(format: POINT_100)
               media {
                 title{english romaji}
                 id
                 staff {
                   edges {
                     role
                     node { id }
                   }
                 }
               }
             }
           }
         },
         User(name: $user){
           favourites {
             staff {
               nodes { id }
             }
             studios {
               nodes { id }
             }
           }
           stats {
             animeListScores {
               meanScore
               standardDeviation
             }
           }
         }"
       vars {:String.user user}
       results (query q vars)]
   {:shows (-> results
               (get-in ["MediaListCollection" "lists"])
               (first)
               (get "entries"))
    :favorites (get-in results ["User" "favourites"])
    :stats (get-in results ["User" "stats"
                            "animeListScores"])}))

(defn get-stddev
  "Return function to compute a score's standard deviation."
  [user-data]
  (let [stats (user-data :stats)]
    (fn [score] (/ (- score (stats "meanScore"))
                   (stats "standardDeviation")))))

(defn process-user-data [data]
  (let [update-score #(update % "score" (get-stddev data))]
    (update data :shows #(map update-score %))))

(defn show-to-staff [show]
  (let [show-info {:title   (get-in show ["media" "title"])
                   :show-id (get-in show ["media" "id"])
                   :score   (get show "score")}]
    (into {} (for [staff (get-in show ["media"
                                       "staff"
                                       "edges"])]
               [(get-in staff ["node" "id"])
                [(assoc show-info :role (staff "role"))]]))))

(defn shows-to-staff
  "Convert query result's show-to-staff mapping to
  a staff-to-show mapping."
  [shows]
  (apply merge-with into (map show-to-staff shows)))

(defn user-preferences [user]
  (-> user
      (get-user-data)
      (process-user-data)))

(defn format-season-show
  [show]
  (assoc show "staff"
         (into {} (for [staff (get-in show ["staff"
                                            "edges"])]
                    [(get-in staff ["node" "id"])
                     {:new-show-role (staff "role")
                      :staff-name (get-in staff ["node" "name"])}]))))

(defn compile-staff [show staff-works]
  (for [[id info] (show "staff")
        :let [works (staff-works id)
              weight (reduce + (map :score works))]
        :when (not (nil? works))]
    (assoc info
           :type :staff
           :works works
           :weight weight)))

(defn compile-list [staff-works]
  (fn [show]
    (assoc show :list
           (concat (compile-staff show staff-works)))))

(defn mean [nums]
  (if (empty? nums)
    0.
    (/ (reduce + nums) (count nums))))

(defn weigh-show [user-data]
  (fn [show]
    (let [types (group-by :type (show :list))
          staff-work-scores (map :weight (types :staff))]
      (assoc show :weight
             (reduce + staff-work-scores)))))

; For debug
(defn save-obj [obj file-name]
  (spit file-name (with-out-str (pr obj))))
(defn load-obj [file-name]
  (read-string (slurp file-name)))

(defn save-season [year season]
  (save-obj
    (get-season year season)
    (str "data/" year "_" (name season) "_season.edn")))
(defn load-season [year season]
  (load-obj
    (str "data/" year "_" (name season) "_season.edn")))

(defn save-user-data [user]
  (save-obj
    (user-preferences user)
    (str "data/" user ".edn")))
(defn load-user-data [user]
  (load-obj
    (str "data/" user ".edn")))

(def season (load-season 2019 :SUMMER))
(def data (load-user-data "my_data"))
(def works (shows-to-staff (:shows data)))
(def results
  (sort-by :weight #(compare %2 %1)
           (map (comp
                  (weigh-show data)
                  (compile-list works)
                  format-season-show)
                season)))
(def b (first (take-last 3 results)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
