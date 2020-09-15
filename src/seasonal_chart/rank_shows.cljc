(ns seasonal-chart.rank-shows
  (:require [clojure.string :as str]))

(defn parse-credit
  [credit]
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
        [_ position eps-str] (re-matches #"([^(]*)(?:\((.+)\))?.*"
                                         credit)
                   ; second contains the first capture group
        eps-list (if eps-str (str/split eps-str #", ?") [])]
    (when (not position) (println credit position eps-str))
    [position (apply concat (map parse-eps eps-list))]))

(defn get-role-weight
  "Given a staff credit position, parse into semantic terms,
   returning [:category weight] where weight is 0-1"
  [credit]
  (let [mapping {"2nd key animation" [:animator 0.5]
                 "action animation director" [:anima-director 1]
                 "animation director" [:anima-director 1]
                 "art design" [:art 1]
                 "art director" [:art 1]
                 "assistant animation director" [:anima-director 0.8]
                 "assistant director" [:direction 0.8]
                 "background art" [:art 1]
                 "cg director" [:cg 1]
                 "character design" [:design 1]
                 "chief animation director" [:anima-director 1]
                 "chief animator" [:animator 1]
                 "chief cg director" [:cg 1]
                 "chief director" [:direction 1]
                 "chief producer" [:production 1]
                 "color design" [:art 0.8]
                 "concept design" [:design 1]
                 "design assistant" [:design 0.7]
                 "design assistance" [:design 0.7]
                 "director" [:direction 1]
                 "director of photography" [:photography 1]
                 "editing" [:direction 0.5]
                 "editing assistant" [:direction 0.2]
                 "editing assistance" [:direction 0.2]
                 "episode director" [:direction 1]
                 "in-between animation" [:animator 0.2]
                 "inserted song performance" [:music 1]
                 "key animation" [:animator 0.9]
                 "layout" [:direction 0.8]
                 "lead character design" [:design 1]
                 "main animation" [:animator 1]
                 "mechanical design" [:design 0.7]
                 "monster design" [:design 0.7]
                 "music" [:music 1]
                 "music producer" [:music 0.5]
                 "original character design" [:design 1]
                 "original creator" [:story 1]
                 "original story" [:story 1]
                 "painted line animation" [:animator 1]
                 "planning" [:production 0.8]
                 "producer" [:production 1]
                 "prop design" [:design 1]
                 "recording" [:sound 0.8]
                 "scenario" [:script 1]
                 "screenplay" [:script 1]
                 "script" [:script 1]
                 "script, series composition" [:series 1]
                 "script assistant" [:script 0.7]
                 "serial composition" [:series 1]
                 "series composition" [:series 1]
                 "setting" [:story 0.5]
                 "sound director" [:sound 1]
                 "sound effects" [:sound 0.7]
                 "storyboard" [:script 0.8]
                 "sub character design" [:design 0.6]
                 "supervis" [:story 0.2]
                 "theme song arrangement" [:music 1]
                 "theme song composition" [:music 1]
                 "theme song lyrics" [:music 1]
                 "theme song performance" [:music 1]
                 "unit director" [:animation 0.5]
                 "world design" [:story 0.7]}
        [position eps-list] (parse-credit credit)
        [category weight] (mapping (-> position
                                       str/trim
                                       str/lower-case)
                                   [:none 0])
        num-eps (if (empty? eps-list) 0 (apply max eps-list))
        ; round up to the nearest multiple of 13
        max-num-eps (+ num-eps (mod (- num-eps) 13))
        proportion-worked (if (pos? num-eps)
                            (/ (count eps-list) max-num-eps)
                            1)]
    (when (not category) (println position))
    ;(when (= category :cg) (println position))
    [category (* proportion-worked weight)]))

(defn rate-role [preference role]
  (let [[pos-type weight] (get-role-weight role)]
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

(defn rate-staff-item
  [preference staff]
  (let [work-rated (update staff :works
                           #(map (partial rate-work preference) %))
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

(defn sort-by-weights [items expected-mean expected-size
                       by-significance]
  (let [bay-avg #(bayesian-average (:weight %)
                                   expected-mean
                                   expected-size)]
  (sort-by (if by-significance
             (comp #(Math/abs %) float bay-avg)
             bay-avg)
           #(compare %2 %1)
           items)))

(defn sort-roles [roles] (sort-by (comp last get-role-weight)
                                  #(compare %2 %1) roles))

; expected-size generated from my own profile
; show a staff's most prominant works instead of highest
; rated works because users want to know if someone made
; shows they've dropped as much as if they made great shows
(defn sort-works [works] (sort-by-weights works 0 2 true))
(defn sort-staff [staff] (sort-by-weights staff 0 8 true))
(defn sort-shows [shows] (sort-by-weights shows -0.29 40 false))
