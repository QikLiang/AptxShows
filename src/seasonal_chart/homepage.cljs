(ns seasonal-chart.homepage
  (:require [reagent.core :as r]
            [reagent.cookies :as cks]
            [ajax.core :refer [GET]]
            [cljs.reader :as reader]
            [clojure.string :as str]
            [seasonal-chart.rank-shows :as rank]))

(def shows (r/atom :loading))

(def settings-version 1)
(if (> settings-version (cks/get :version 0))
  (do
    (cks/set! :version settings-version)
    (cks/remove! :settings)))

(def default-settings
  {:preference {:story 1, :sound 0.5,
                :art 0.5, :cg 0.2,
                :music 0.5, :direction 1,
                :design 0.5, :animation 1,
                :production 0.2}
   :auto-update true
   :remember-preference true
   :filters {:adult false
             :empty true
             :planning true
             :watching true
             :paused true
             :completed false
             :dropped false}})
(def settings-ui
  (r/atom (cks/get :settings default-settings)))

(def settings (r/atom @settings-ui))

(def url-hash (as-> js/window.location.hash $
                (if (< (count $) 2) "#default=param" $)
                (subs $ 1)
                (str/split $ "&")
                (map #(str/split % "=" -1) $)
                (map #(update % 0 keyword) $)
                (into {} $)))

(def seasons-list [{:year "2019" :season "summer"}
                   {:year "2019" :season "fall"}
                   {:year "2020" :season "winter"}
                   {:year "2020" :season "spring"}
                   {:year "2020" :season "summer"}])

(def cur-season
  (r/atom (merge (cks/get :cur-season (last seasons-list))
                 url-hash)))

(def username (->> ""
                   (cks/get :username)
                   (get url-hash :username)
                   (js/decodeURIComponent)
                   (r/atom)))

(def init-items 5)

(def show-items (r/atom init-items))

(defn update-shows! []
  (js/console.log (str (:preference @settings)))
  (reset! settings @settings-ui)
  (when (:remember-preference settings)
    (cks/set! :settings @settings)))

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
    (when (:remember-preference @settings-ui)
      (cks/set! :settings @settings-ui)
      (cks/set! :cur-season @cur-season)
      (cks/set! :username user))
    (reset! shows :loading)
    (reset! show-items init-items)
    (GET url {:handler reset-state})
    (set! js/window.location.href
          (str "#year=" (@cur-season :year)
               "&season=" (@cur-season :season)
               "&username=" (js/encodeURIComponent @username)))))

(defn r-map
  "a version of map that puts functions in a form that
   reagent can call afterwards"
  ([f vs] (r-map identity f vs))
  ([keyf f vs]
   (map (fn [v] ^{:key (keyf v)}[f v]) vs)))

(defn show-season-button [{:keys [year season] :as entry}]
  [:a
   {:on-click (fn [_]
                (reset! cur-season entry)
                (get-shows!))
    :href (str "/#year=" year
               "&season=" season
               "&username=" @username)
    :class (str "button season-button"
                (if (and (= (@cur-season :year) (str year))
                         (= (@cur-season :season) season))
                  " selected-season" ""))}
   (str(str/upper-case season) " " year)])

(defn setting-slider [[desc param]]
  [:div.setting-slider
   [:div.slider-desc desc]
   (let [slider-range 100]
     [:input {:type "range" :min 0 :max slider-range
              :value (* slider-range
                        (get-in @settings-ui [:preference param]))
              :on-change (fn [e]
                           (do
                             (swap! settings-ui assoc-in
                                    [:preference param]
                                    (/ (.. e -target -value)
                                       slider-range))
                             (auto-update!)))}])])

(defn setting-checkbox
  [settings-path description element-id]
    [:div.checkbox-group
     {:on-click (fn [_]
                  (swap! settings-ui update-in
                         settings-path not)
                  (auto-update!))}
     [:input
      {:type "checkbox"
       :id element-id
       :read-only true
       :checked (get-in @settings-ui settings-path)}]
     [:div.checkbox-desc description]])

(defn show-header []
  [:div#header-content
   [:div#seasons-list
    (r-map show-season-button seasons-list)]
   [:hr]
   [:div#user-entries
    [:form#anilist-entry.user-entry
     {:on-submit get-shows!}
     "Anilist unsername: "
     [:input#anilist-input {:type "text"
                            :value @username
                            :on-change
                            #(reset! username
                                     (-> %
                                         .-target
                                         .-value))}]
     [:button.button "Fetch my profile"]]
    [:p#myanimelist-entry
     "MyAnimeList: Support work in progress.\u00A0"
     [:a {:href "https://anilist.co/forum/thread/3393"}
      " In the meantime, it's possible to export data
       to an Anilist account."]]]
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
   (into [:div#filter-buttons]
         (for [[k label] [[:empty "empty entries"]
                          [:adult "adult content"]
                          [:planning "in Planning list"]
                          [:watching "in Watching list"]
                          [:completed "in Completed list"]
                          [:dropped "in Dropped list"]]]
           (setting-checkbox [:filters k] label
                             (str/replace label " " "-"))))
   [:hr]
   [:div#update-button-div
    [:button#update-button.button {:on-click update-shows!}
     "Apply Preferences"]
    (setting-checkbox [:remember-preference]
                      "Remember my preferences"
                      "remember-preference")
    (setting-checkbox [:auto-update]
                      "Auto-update (uncheck if laggy)"
                      "auto-update")]])

(defn abreviate-title
  "insert ellipses smartly in long show titles"
  [title]
  (if (< (count title) 30)
    title
    (let [
          last-space (str/last-index-of title " ")
          abrev-end (max (- (count title) 10)
                         (+ last-space 1))
          ; minus 3 for the ellipses
          max-abrev-start (- 25 (- (count title) abrev-end 3))
          abrev-start (max
                        (str/last-index-of
                          title " " max-abrev-start)
                        (- max-abrev-start 5))
          ]
    (str (subs title 0 abrev-start)
         "\u200B...\u200B"
         (subs title abrev-end)))))

(defn abreviate-role
  "Remove () at the end of a role for clarity"
  [role]
  (let [paren-ind (str/last-index-of role "(")]
    (if paren-ind
      (subs role 0 paren-ind)
      role)))

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
  [:a.entity-group attrs
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
                  (->> (show :roles)
                       (rank/sort-roles)
                       (take 2)
                       (map abreviate-role))
                  {:key (show :show-id)
                   :class "show-entity"
                   :href (str "https://anilist.co/anime/"
                              (show :show-id))}))

(defn display-staff-entity [staff]
  (display-entity (staff :staff-img)
                  (print-name (staff :staff-name))
                  (rank/sort-roles (staff :roles))
                  {:class "staff-entity"
                   :href (str "https://anilist.co/staff/"
                              (staff :staff-id))}))

(defn display-staff-works [staff]
  [:div.list-item.staff-works
   ^{:key (select-keys staff [:staff-id :weight])}
   [display-staff-entity staff]
   (r-map display-show-entity
        (take 4 (rank/sort-works (staff :works))))])

(defn filter-show
  "Just like the convention for filter, this returns false
   if the show should be hidden."
  [filters show]
  (and (or (:adult filters) (not (show "isAdult")))
       (or (:empty filters) (seq (show :list)))
       (let [status-label {"PLANNING" :planning
                           "COMPLETED" :completed
                           "PAUSED" :paused
                           "CURRENT" :watching
                           "DROPPED" :dropped}]
         (get filters (status-label (:status show)) true))))

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
                  (take 4 (rank/sort-staff (show :list)))))]])

(defn scroll-end []
  (r/after-render #(.scrollTo
                     js/window 0
                     (.. js/document -body -offsetHeight)))
  (reset! show-items (count @shows)))

(defn scroll-end-button []
  [:svg.scroll-end-button {:width 50 :height 50
                           :on-click scroll-end}
   [:polyline {:points "7,15 25,30 42,15"}]
   [:line {:x1 7 :y1 35 :x2 42 :y2 35}] ])

(defn show-list []
  (cond
    (= @shows :loading)
    [:h1.show-item.load-message "Please wait while loading"]

    (= @shows :unhandled-error)
    [:h1.show-item.load-message "Something went wrong.
                                 Please wait for it to be fixed."]

    (= @shows :user-not-found)
    [:div.show-item.error-message
     [:h1 "Username not found."]
     [:hr]
     [:p "This website gives recommendations based on what it"
      "can understand from your "
      [:a {:href "anilist.co"} "Anilist"]
      " profile. However, there is no Anilist user named "
      \" @username \"]
     [:p
      [:a.button
       {:href (str "/#year=" (@cur-season :year)
                   "&season=" (@cur-season :season)
                   "&username=")
        :on-click #(update-shows!)}
       "Back to default view"]
      "The default view makes recommendations based on the "
      "top 1,000 most popular shows."]
     [:h3 "For MyAnimeList users"]
     [:p "It is possible to create an Anilist profile and "
      "export the shows you've seen from MAL. "
      [:a {:href "https://anilist.co/forum/thread/3393"}
       "See here for instructions."]]]

    :else
    (into [:div.show-list (scroll-end-button)]
          (->> @shows
               (map (partial rank/apply-preference
                             (:preference @settings)))
               (rank/sort-shows)
               (filter (partial filter-show
                                (:filters @settings)))
               (take @show-items)
               (r-map #(select-keys % ["id" :weight])
                      display-show)))))

(r/render [show-header]
          (.getElementById js/document "header"))

(r/render [show-list] (.getElementById js/document "app"))
(get-shows!)

(defn check-scroll []
  (when (and
           (seq? @shows)
           (< @show-items (count @shows))
           (> (+ (.-scrollY js/window) (.-innerHeight js/window))
              (- (.. js/document -body -offsetHeight) 3000)))
    (swap! show-items + init-items)))

(.setInterval js/window check-scroll 1000)
