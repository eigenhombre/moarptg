(defproject moarptg "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :aliases {"sprightly" ["run" "-m" "moarptg.render/sprightly"]}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [clojure-lanterna "0.9.7"]]
  :main moarptg.core
  :profiles {:uberjar {:aot :all}}
  :uberjar-name "moarptg.jar")
