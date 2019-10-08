(require '[seasonal-chart.anilist-api :refer :all])

(save-season 2019 :SUMMER)
(save-season 2019 :FALL)
(save-obj (get-popular) "data/most_popular.edn")
