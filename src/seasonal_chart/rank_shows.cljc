(ns seasonal-chart.rank-shows
  (:require [clojure.string :as str]))

(defn parse-num-eps
  [list-str]
  (let [parse-num #(let [val #?(:clj (read-string %)
                                :cljs (js/parseInt % 10))]
                     (if (integer? val) val 1))
        parse-eps-regex #"[^\d]*(\d+|op|ed|OP|ED)(?:-(\d+))?"
        parse-eps (fn [eps]
                    (let [[whole-str start end]
                          (re-matches parse-eps-regex eps)]
                      (cond
                        ; regex no match
                        (nil? whole-str) [0]
                        ; only match one number
                        (nil? end) [(parse-num start)]
                        ; match range, e.g. 1-15
                        :else
                        (into [] (range (parse-num start)
                                        (+ 1 (parse-num end)))))))
        in-parens (re-matches #".*\((.+)\)" list-str)
                   ; second contains the first capture group
        eps-list (if (nil? in-parens) []
                   (str/split (second in-parens) #", ?"))]
    (apply concat (map parse-eps eps-list))))

(defn bsearch
  "binarysearch function."
  ([x ls compare-f] (bsearch x ls 0 (dec (count ls))
                              compare-f))
  ([x ls low high compare-f]
    (if (< high low) -1 ; not in collection
    (let [middle (quot (+ low high) 2)
         v (nth ls middle)]
      (cond (= 0 (compare-f x v)) middle
            (> 0 (compare-f x v)) (recur x ls low (dec middle)
                                         compare-f)
            (< 0 (compare-f x v)) (recur x ls (inc low)
                                         high compare-f))))))

(defn parse-position
  "Given a staff credit position, parse into semantic terms,
   returning [:position weight] where weight is 0-1"
  [position]
  (let [pos (str/lower-case position)
        mapping [["animation" :animation 1]
                 ["art" :art 1]
                 ["assistant animation" :animation 0.8]
                 ["assistant director" :direction 0.8]
                 ["background art" :art 1]
                 ["cg" :cg 1]
                 ["character", :design 1]
                 ["chief animation" :animation 1]
                 ["chief director" :direction 1]
                 ["chief producer" :production 1]
                 ["color" :art 1]
                 ["director" :direction 1]
                 ["editing" :direction 0.8]
                 ["episode director" :direction 1]
                 ["in-between" :animation 0.2]
                 ["key animation" :animation 0.8]
                 ["lead character" :design 1]
                 ["main animat" :animation 1]
                 ["music" :music 1]
                 ["original character" :design 1]
                 ["original creator" :story 1]
                 ["original story" :story 1]
                 ["planning" :production 0.8]
                 ["producer" :production 1]
                 ["prop design" :design 1]
                 ["recording" :sound 0.8]
                 ["scenario" :story 1]
                 ["screenplay" :story 1]
                 ["script" :story 1]
                 ["serial composition" :story 1]
                 ["series composition" :story 1]
                 ["setting" :story 0.5]
                 ["sound" :sound 1]
                 ["storyboard" :story 0.8]
                 ["sub character" :design 0.6]
                 ["supervis" :story 0.2]
                 ["theme song" :music 1]
                 ["unit director" :animation 0.5]]
        ; lexigraphically compare until one string runs out
        str-compare (fn [sa b] ; a is string, b still in list
                      (let [sb (first b)
                            len (apply min
                                       (map count [sa sb]))]
                        (apply compare
                               (map #(subs % 0 len)
                                    [sa sb]))))
        index (bsearch (str/lower-case position)
                mapping str-compare)
        eps-list (parse-num-eps position)
        num-eps (if (empty? eps-list) 0 (apply max eps-list))
        ; round up to the nearest multiple of 13
        max-num-eps (+ num-eps (mod (- num-eps) 13))
        ]
    (cond
      (< index 0) [position :none 0]
      (<= num-eps 0) (nth mapping index)
      :else (update (nth mapping index) 2
                    * (/ (count eps-list) max-num-eps)))))

;(def preference {:story 1
                 ;:sound 1
                 ;:art 1
                 ;:music 1
                 ;:direction 1
                 ;:design 1
                 ;:animation 1
                 ;:production 1
                 ;:cg 1})

(defn rate-role [preference role]
  (let [[pos pos-type weight] (parse-position role)]
    {:weight (* weight (preference pos-type))
     :count 1}))
(def combine-weights (partial merge-with +))
(defn rate-roles [preference roles]
  (reduce combine-weights
          (map (partial rate-role preference) roles)))

(defn rate-work [preference work]
  (-> work
      (assoc :weight (rate-roles preference (:roles work)))
      (update-in [:weight :weight] * (:score work))))
(defn rate-works [preference works]
  (map (partial rate-work preference) works))

(defn rate-staff-item
  [preference staff]
  (let [work-rated (update staff :works
                           (partial rate-works preference))
        works-weight (reduce combine-weights
                             (map :weight
                                  (:works work-rated)))
        cur-roles-weight (rate-roles preference
                                     (:roles staff))]
    (assoc work-rated :weight
           (update works-weight :weight *
                   (:weight cur-roles-weight)))))

(defn rank-item
  "update the score in a single :list entry of a show"
  [preference item]
  (case (:type item)
    :staff (rate-staff-item preference item)
    ; unchanged if type unmatched
    item))

(defn apply-preference
  "given a preference object, create a function that will
   judge a show (alter its score) based on the preference"
  [preference show]
  (as-> show s
      (update s :list #(map (partial rank-item preference) %))
      (assoc s :weight (reduce combine-weights
                               (map :weight (:list s))))))

(defn bayesian-average
  "See e.g. https://fulmicoton.com/posts/bayesian_rating/
   Given a certain number of observations, Bayesian
   statistics predict that the true mean should be
   approximated as:
   true_mean = (expected_size*expected_mean
                + num_observations*observations_mean)
               / (num_observations + expected_size)
   where expected_size is the average number of observations
   and expected_mean is the prior belief of what the mean
   should be. Since scores are adjusted to be standard
   deviations from the mean, the expected mean should, in
   theory, be zero. However, emperically I've found that to
   not be the case because there's a correlation where staff
   that made good works likely get more jobs. Also, it makes
   sense to penaltize shows with a staff list you know
   nothing about (or empty). This is done by assigning a
   negative expected mean."
  [{w :weight c :count} expected-mean expected-size]
  (/ (+ (* w c) (* expected-mean expected-size))
     (+ c expected-size)))

(defn sort-by-weights [items expected-mean expected-size]
  (sort-by #(bayesian-average (:weight %)
                              expected-mean expected-size)
           #(compare %2 %1)
           items))

(defn sort-roles [roles] (sort-by parse-position
                                  #(compare %2 %1) roles))

; expected-size generated from my own profile
(defn sort-works [works] (sort-by-weights works 0 2))
(defn sort-staff [staff] (sort-by-weights staff 0 8))
(defn sort-shows [shows] (sort-by-weights shows -0.29 40))
