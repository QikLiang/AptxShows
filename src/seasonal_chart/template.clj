(ns seasonal-chart.template
  (:require [hiccup.page :refer [html5 include-js
                                 include-css]]))

(defn home-page []
  (html5
    [:head
     [:title "APTX Shows - Condensing the dozens of shows
             per season to the few you actually care about"]
     (include-css "css/style.css")]
    [:body
     [:div#center-strip
      [:div#header]
      [:div#app
       [:h1.show-item.load-message
        "Please wait while loading"]]]
     (include-js "cljs-out/homepage.js")]))
