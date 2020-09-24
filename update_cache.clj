(require '[seasonal-chart.anilist-api :refer [save-season
                                              get-popular
                                              save-obj]]
         '[seasonal-chart.time-utils :as time])

(let [minute (* 60 1000) ; in ms
      hour (* 60 minute)
      first-season {:year 2019 :season 2}
      ; compute data 3 days ahead in case server client
      ; timezone differents
      cur-season (time/cur-season 3)
      latest-season (time/inc-season cur-season)
      seasons (mapv time/season->str
                    (time/season-range first-season latest-season))]
  (doseq [{year :year season :season} seasons]
    (println "saving" year season)
    (save-season year season)
    (Thread/sleep minute)))


;(save-season 2019 :FALL)
;(save-season 2020 :WINTER)
;(save-obj (get-popular) "data/most_popular.edn")
