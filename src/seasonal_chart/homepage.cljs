(ns seasonal-chart.homepage
  (:require [reagent.core :as r]
            [ajax.core :refer [GET]]
            [cljs.reader :as reader]
            [clojure.string :as str]))

(js/console.log "testing")

(def shows (r/atom []))

(defn reset-state [showlist]
  (js/console.log showlist)
  (reset! shows (reader/read-string showlist)))

(GET "/api/user" {:handler reset-state})

(defn abreviate-title
  "insert ellipses smartly in long show titles"
  [title]
  (if (< (count title) 30)
    title
    (let [abrev-start (or
                        (str/last-index-of title " " 15)
                        10)
          last-space (str/last-index-of title " ")
          abrev-end (max (- (count title) 10)
                         last-space)]
    (str (subs title 0 abrev-start)
         " ... "
         (subs title abrev-end)))))

(defn print-name
  "Convert first and last name into string for display"
  [entity]
  (str/join
    ", "
    (filter some? [(entity "last") (entity "first")])))

(defn display-entity
  "Create a display with an image, name, and description"
  [ent-img ent-name ent-desc]
  ; {} for parent functions to add attributes
  [:div.entity-group {}
   [:img {:src ent-img}]
   [:div.entity-name ent-name]
   [:div.entity-desc (if (seq? ent-desc)
                       (for [desc ent-desc]
                         [:div desc])
                       ent-desc)]])

(defn display-show-entity [show]
  (assoc (display-entity (show :image)
                         (abreviate-title
                           (get-in show [:title "romaji"]))
                         (show :roles))
         1 {:class "show-entity"}))

(defn display-staff-entity [staff]
  (assoc
    (display-entity (staff :staff-img)
                    (print-name (staff :staff-name))
                    (for [role (staff :roles)]
                      [:div role]))
    1 {:class "staff-entity"}))

(defn display-staff-works [staff]
    [:div.list-item.staff-works
     (display-staff-entity staff)
     (map display-show-entity
          (take 4 (sort-by :score (comp - compare)
                           (staff :works))))])

(defn display-show [show]
  [:div.show-item
   [:div.cover-wrapper
    [:img.show-img {:src
                    (get-in show ["coverImage" "large"])}]
    [:div.show-info
     [:div.show-title (get-in show ["title" "romaji"])]
     (into [:div.show-info-list]
           (map display-staff-works
                (take 4 (sort-by :weight (comp - compare)
                                 (show :list)))))]]])

(defn show-list []
  (if (empty? @shows) [:div "empty"]
    (into [:div.show-list] (map display-show @shows))))

(r/render [show-list] (.getElementById js/document "app"))
