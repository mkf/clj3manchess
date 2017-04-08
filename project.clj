(defproject clj3manchess "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.9.494"]
                 [prismatic/schema "1.1.5"]]
  :plugins [[lein-cljsbuild "1.1.5"]
            [lein-figwheel "0.5.9"]
            [lein-doo "0.1.7"]
            [lein-bikeshed "0.2.0"]
            [lein-cljfmt "0.5.6"]
            [lein-kibit "0.1.3"]]
  :source-paths ["src"]
  :test-paths ["test"]
  :cljsbuild
  {:builds {
   :dev {:source-paths ["src"]
          :id "main"
         :jar true
         :compiler {:output-to "target/main.js"
                    :output-dir "target"
                    :optimizations :none
                    :pretty-print true}}
   :test {:source-paths ["src" "test"]
           :incremental true
           :compiler {:output-to "target/main-test.js"
                      :output-dir "target-test"
                      :optimizations :whitespace
                      :pretty-print true}}}})

