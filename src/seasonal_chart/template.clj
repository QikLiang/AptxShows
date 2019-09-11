(ns seasonal-chart.template
  (:require [hiccup.page :refer :all]))

(defn home-page []
  (html5
    [:head
     [:title "Home Page"]
     (include-css "css/style.css")]
    [:body
     [:div#center-strip
      [:div#header]
      [:div#app]]
     (include-js "cljs-out/homepage.js")]))
