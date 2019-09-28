(ns seasonal-chart.homepage
  (:require [reagent.core :as r]
            [ajax.core :refer [GET]]
            [cljs.reader :as reader]
            [clojure.string :as str]
            [seasonal-chart.rank-shows :as rank]))

(def shows (r/atom []))

(def settings-ui (r/atom {:preference
                          {:story 1, :sound 1, :art 1, :cg 1,
                           :music 1, :direction 1, :design 1,
                           :animation 1, :production 1}
                          :auto-update true}))

(def settings (r/atom @settings-ui))

(def cur-season (atom {:year 2019 :season "summer"}))

(defn update-shows! []
  (js/console.log (str (:preference @settings)))
  (reset! settings @settings-ui))

(defn auto-update! []
  (when (:auto-update @settings-ui) (update-shows!)))

(defn reset-state [showlist]
  (reset! shows (reader/read-string showlist)))

(defn get-shows! []
  (let [{year :year season :season} @cur-season
        user (some-> js/document
                 (.getElementById "anilist-input")
                 (.-value)
                 (js/encodeURIComponent))
        url (str/join "/" ["/api/getshows"
                           (str year) season user])]
  (GET url {:handler reset-state})))
;(GET "/api/user" {:handler reset-state})

(defn r-map
  "a version of map that puts functions in a form that
   reagent can call afterwards"
  ([f vs] (r-map identity f vs))
  ([keyf f vs]
   (map (fn [& v] (into ^{:key (keyf v)}[f] v)) vs)))

(defn show-season-button [{:keys [year season] :as entry}]
  [:button.button.season-button
   {:on-click (fn [e] (do
                        (reset! cur-season entry)
                        (get-shows!)))}
   (str(str/upper-case season) " " year)])

(defn setting-slider [[desc param]]
  [:div.setting-slider
   [:div.slider-desc desc]
   (let [slider-range 100]
     [:input {:type "range" :min 0 :max slider-range
              :value (* slider-range
                        (get-in @settings-ui [:preference param]))
              :on-change (fn [e]
                           (swap! settings-ui assoc-in
                                  [:preference param]
                                  (/ (.. e -target -value)
                                     slider-range)))
              :on-mouse-up auto-update!
              :on-touch-end auto-update!}])])

(defn show-header []
  [:div#header-content
   [:div#seasons-list
    (r-map show-season-button [{:year 2019 :season "summer"}
                               {:year 2019 :season "fall"}])]
   [:hr]
   [:div#user-entries
    [:div#anilist-entry.user-entry
     "Anilist unsername: "
     [:input#anilist-input {:type "text"}]
     [:button "Fetch my completed list"]]]
   [:hr]
   [:div#settings-sliders
    (r-map setting-slider
           [["Directing"     :direction]
            ["Animation"     :animation]
            ["Story"         :story]
            ["Art"           :art]
            ["Music"         :music]
            ["Visual Design" :design]
            ["Sound"         :sound]
            ["CG"            :cg]
            ["Production"    :production]])]
   [:hr]
   [:div#update-button-div
    [:button#update-button.button {:on-click update-shows!}
     "Apply Preferences"]
    [:input#auto-update
     {:type "checkbox"
      :checked (:auto-update @settings-ui)
      :on-change (fn [e]
                   (swap! settings-ui update :auto-update
                          not))}]
    [:div "Auto-update (uncheck if laggy)"]]])

(defn abreviate-title
  "insert ellipses smartly in long show titles"
  [title]
  (if (< (count title) 30)
    title
    (let [abrev-start (or
                        (str/last-index-of title " " 15)
                        10)
          last-space (str/last-index-of title " ")
          abrev-end (max (- (count title) 10)
                         last-space)]
    (str (subs title 0 abrev-start)
         " ... "
         (subs title abrev-end)))))

(defn print-name
  "Convert first and last name into string for display"
  [entity]
  (str/join
    ", "
    (filter some? [(entity "last") (entity "first")])))

(defn display-entity
  "Create a display with an image, name, and description"
  [ent-img ent-name ent-desc attrs]
  ; {} for parent functions to add attributes
  [:a.entity-group (merge {:key ent-name} attrs)
   [:img {:src ent-img}]
   [:div.entity-name ent-name]
   (into [:div.entity-desc] (if (sequential? ent-desc)
                              (map (partial into [:p])
                                   ent-desc)
                              [ent-desc]))])

(defn display-show-entity [show]
  (display-entity (show :image)
                  (abreviate-title
                    (get-in show [:title "romaji"]))
                  (show :roles)
                  {:class "show-entity"
                   :href (str "https://anilist.co/anime/"
                              (show :show-id))}))

(defn display-staff-entity [staff]
  (display-entity (staff :staff-img)
                  (print-name (staff :staff-name))
                  (staff :roles)
                  {:class "staff-entity"
                   :href (str "https://anilist.co/staff/"
                              (staff :staff-id))}))

(defn display-staff-works [staff]
  [:div.list-item.staff-works
   (display-staff-entity staff)
   (map display-show-entity
        (take 4 (sort-by :score (comp - compare)
                         (staff :works))))])

(defn display-show [show]
  [:div.show-item {:key (show "id")}
   [:div.info-column
    [:img.show-img {:src
                    (get-in show ["coverImage" "large"])}]
    [:a {:href (str "https://anilist.co/anime/" (show "id"))}
     "Anilist entry"]
    [:a {:href (str "https://myanimelist.net/anime/"
                    (show "idMal"))}
     "MyAnimeList entry"]]
    [:div.show-info
     [:div.show-title (get-in show ["title" "romaji"])]
     (into [:div.show-info-list]
           (r-map :staff-name display-staff-works
                  (take 4 (sort-by :weight (comp - compare)
                                   (show :list)))))]])

(defn show-list []
  (if (empty? @shows) [:h1.show-item.load-message
                       "Please wait while loading"]
    (into [:div.show-list]
          (->> @shows
               (map (partial rank/apply-preference
                             (:preference @settings)))
               (sort-by :weight (comp - compare))
               (r-map #(get % "id") display-show)))))

(r/render [show-header]
          (.getElementById js/document "header"))

(r/render [show-list] (.getElementById js/document "app"))
(get-shows!)
