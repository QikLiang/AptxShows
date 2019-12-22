(require '[seasonal-chart.anilist-api :refer :all])

(save-season 2019 :FALL)
(save-season 2020 :WINTER)
(save-obj (get-popular) "data/most_popular.edn")
