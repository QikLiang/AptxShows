(ns seasonal-chart.time-utils)

(defn cur-year-month
  "return {:year :month} with zero-indexed month"
  ([] (cur-year-month 0))
  ([offset]
  #?(:clj (let [date (. (java.time.LocalDate/now)
                        plusDays offset)]
            {:year (. date getYear)
             :month (dec (.. date getMonth getValue))})
     :cljs (let [date (new js/Date)]
             ; the ugly way of incrementing dates in JS
             (. date setDate (+ (. date getDate) offset))
             {:year (. date getFullYear)
              :month (. date getMonth)}))))

(defn month->season [{year :year month :month}]
  {:year year :season (quot month 3)})

(def cur-season (comp month->season cur-year-month))

(defn inc-season [{year :year season :season}]
  (if (= season 3)
    {:year (inc year) :season 0}
    {:year year :season (inc season)}))

(defn season<= [{y1 :year s1 :season} {y2 :year s2 :season}]
  (if (= y1 y2)
    (<= s1 s2)
    (<= y1 y2)))

(defn season-range [begin end]
    (take-while #(season<= % end) (iterate inc-season begin)))

(defn season->str [{year :year season :season}]
  {:year (str year)
   :season (nth ["winter" "spring" "summer" "fall"] season)})
