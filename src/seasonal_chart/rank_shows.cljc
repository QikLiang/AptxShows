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
                 ["chief producer" :production 1]
                 ["color" :art 1]
                 ["director" :direction 1]
                 ["editing" :direction 0.8]
                 ["episode director" :direction 1]
                 ["in-between" :animation 0.2]
                 ["key animation" :animation 0.8]
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
                 ["sound" :sound 1]
                 ["storyboard" :story 0.8]
                 ["supervis" :story 0.5]
                 ["theme song" :music 1]
                 ["unit director" :direction 1]]
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
    (* weight (preference pos-type))))

(defn rate-staff-item
  [preference staff]
  (let [rate-roles
        #(reduce + (map (partial rate-role preference) %))
        rate-work #(update % :score * (rate-roles (:roles %)))
        work-rated (update staff :works #(map rate-work %))]
    (update work-rated :weight *
            (reduce + (map :score (:works work-rated)))
            (rate-roles (:roles work-rated)))))

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
      (assoc s :weight (reduce + (map :weight (:list s))))))
