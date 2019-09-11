(ns seasonal-chart.handler
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
            [ring.middleware.defaults
             :refer [wrap-defaults site-defaults]]
            [org.httpkit.server :refer [run-server]]
            [seasonal-chart.template :refer [home-page]]
            [ring.middleware.content-type
             :refer [wrap-content-type]]
            [seasonal-chart.anilist-api :as anilist]))

(defroutes app-routes
  (GET "/" a (home-page))
  (GET "/api/user" [] (pr-str (anilist/load-results)))
  (route/resources "/" )
  (route/not-found "Not Found"))

(def app
    (wrap-defaults
  ;(wrap-content-type app-routes)
  app-routes
      site-defaults))

(defonce server (atom nil))

(defn start []
  (reset! server
          (run-server #'app {:port 3000 :join false})))

(defn -main [& args]
  (start))