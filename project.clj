(defproject seasonal-chart "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [cljs-ajax "0.7.5"]
                 [org.clojure/data.json "0.2.6"]
                 [org.clojure/core.async "0.4.500"]]
  :main ^:skip-aot seasonal-chart.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
