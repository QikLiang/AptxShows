(defproject seasonal-chart "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :resource-paths ["target" "resources"]
  :target-path "target/%s/"
  :clean-targets ^{:protect false} [:target-path]
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/clojurescript "1.10.312"]
                 [cljs-ajax "0.7.5"]
                 [org.clojure/core.async "0.4.500"]
                 [http-kit "2.3.0"]
                 [compojure "1.6.1"]
                 [ring/ring-defaults "0.3.2"]
                 [hiccup "1.0.5"]
                 [reagent "0.8.1"]
                 [reagent-utils "0.3.3"]]
  :main seasonal-chart.handler
  :plugins [[lein-ring "0.12.5"]
            [lein-cljsbuild "1.1.7"]
            [lein-exec "0.3.7"]]
  :ring {:handler seasonal-chart.handler/app}
  :cljsbuild { :builds [{:source-paths ["src"]
                         :compiler {:output-to "target/public/cljs-out/homepage.js"
                                    :optimizations :advanced}}]}
  :profiles
  {:dev {:dependencies [[javax.servlet/servlet-api "2.5"]
                        [ring/ring-mock "0.3.2"]
                        [com.bhauman/figwheel-main "0.2.3"]]}
   :uberjar {:aot [seasonal-chart.handler
                   seasonal-chart.anilist-api
                   seasonal-chart.template]
             }}
  :aliases {"fig"       ["trampoline" "run" "-m" "figwheel.main"]
            "fig:build" ["trampoline" "run" "-m" "figwheel.main" "-b" "dev" "-r"]
            "recache"   ["exec" "-p" "update_cache.clj"]})
