(ns seasonal-chart.handler
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
            [ring.middleware.defaults
             :refer [wrap-defaults site-defaults]]
            [org.httpkit.server :refer [run-server]]
            [seasonal-chart.template :refer [home-page]]
            [ring.middleware.content-type
             :refer [wrap-content-type]]
            [seasonal-chart.anilist-api :as anilist]
            [clojure.java.io :refer [writer]]
            [clojure.core.async :as a]
            [clojure.string :as str])
  (:gen-class))

(def errchan (a/chan))

(a/go
  (with-open [wrtr (writer
                     (str "error_log/"
                          (str/replace
                            (.toString (java.util.Date.))
                            " " "_")
                          ".log")
                     :append true)]
    (loop []
      (when-let [v (a/<! errchan)]
        (.write wrtr (str v "\n"))
        (.flush wrtr)
        (recur)))))

(defroutes app-routes
  (GET "/" a (home-page))
  (GET "/api/getshows/:year/:season/"
       [year season] (pr-str (do
                               (println [year season])
                               (anilist/load-results
                                 year season))))
  (GET "/api/getshows/:year/:season/:user"
       [year season user] (pr-str (do
                                    (println [year season
                                              user])
                                    (try
                                    (anilist/load-results
                                      year season user)
                                    (catch Exception e
                                      (a/go
                                        (a/>! errchan
                                              (str/join
                                                [year season
                                                 user] "/")))
                                      :unhandled-error)))))
  (route/resources "/" )
  (route/not-found "Not Found"))

(def app
    (wrap-defaults
  ;(wrap-content-type app-routes)
  app-routes
      site-defaults))

(defonce server (atom nil))

(def port (Integer/valueOf
            (or (System/getenv "port")
                "3000")))

(defn start []
  (reset! server
          (run-server #'app {:port port})))

(defn -main [& args]
  (println (str "starting server on port " port))
  (start)
  (println "started"))
