(defproject seasonal-chart "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :resource-paths ["target" "resources"]
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [cljs-ajax "0.7.5"]
                 [org.clojure/core.async "0.4.500"]
                 [compojure "1.6.1"]
                 [ring/ring-defaults "0.3.2"]
                 [org.clojure/clojurescript "1.10.312"]
                 [http-kit "2.3.0"]
                 [hiccup "1.0.5"]]
  :main seasonal-chart.handler
  :plugins [[lein-ring "0.12.5"]]
  :ring {:handler seasonal-chart.handler/app}
  :profiles
  {:dev {:dependencies [[javax.servlet/servlet-api "2.5"]
                        [ring/ring-mock "0.3.2"]
                        [com.bhauman/figwheel-main "0.2.3"]
                        ;[com.bhauman/rebel-readline-cljs "0.1.4"]
                        ]}}
  :aliases {"fig"       ["trampoline" "run" "-m" "figwheel.main"]
            "fig:build" ["trampoline" "run" "-m" "figwheel.main" "-b" "dev" "-r"]})
