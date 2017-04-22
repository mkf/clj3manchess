(defproject clj3manchess "0.3.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.9.0-alpha15"]
                 [org.clojure/clojurescript "1.9.494"]
                 [clj3manchess/engine "0.3.0-SNAPSHOT"]
                 [clj3manchess/online-core "0.3.0-SNAPSHOT"]
                 [clj3manchess/online-client "0.3.0-SNAPSHOT"]
                 [clj3manchess/online-server "0.3.0-SNAPSHOT"]]
  :plugins [[lein-sub "0.3.0"]
            [lein-cljsbuild "1.1.5"]
            [lein-figwheel "0.5.9"]
            [lein-doo "0.1.7"]
            [lein-bikeshed "0.2.0"]
            [lein-cljfmt "0.5.6"]
            [lein-kibit "0.1.3"]]
  :sub ["engine"
        "online/core"
        "online/client"])
       ;; "online/server"])

