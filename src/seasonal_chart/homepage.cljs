(ns seasonal-chart.homepage
  (:require [reagent.core :as r]
            [ajax.core :refer [GET]]
            [cljs.reader :as reader]))

(js/console.log "testing change")

(def state (r/atom []))

(defn reset-state [new-state]
  (reset! state (reader/read-string new-state)))

(GET "/api/user" {:handler reset-state})

(defn div []
  [:div "asdf"])

(defn show-list []
  (if (empty? @state) [:div "empty"]
  (into [:div] (for [show @state]
  [:div (get-in show ["title" "romaji"])]))))

(r/render [show-list] (.getElementById js/document "app"))
