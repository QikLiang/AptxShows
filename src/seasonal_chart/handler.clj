(ns seasonal-chart.handler
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
            [ring.middleware.defaults
             :refer [wrap-defaults site-defaults]]
            [seasonal-chart.template :refer [home-page]]
            [ring.middleware.content-type
             :refer [wrap-content-type]]
            [seasonal-chart.anilist-api :as anilist]))

(defroutes app-routes
  (GET "/" a (home-page))
  (GET "/api/user" [] (pr-str anilist/results))
  (route/resources "/" )
  (route/not-found "Not Found"))

(def app
    (wrap-defaults
  ;(wrap-content-type app-routes)
  app-routes
      site-defaults))
