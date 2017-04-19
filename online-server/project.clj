(defproject clj3manchess.online.server "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :plugins [[lein-ring "0.8.11"]]
  :ring {:handler clj3manchess.online.server/handler}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [clj3manchess "0.2.0-SNAPSHOT"]
                 [liberator "0.14.1"]
                 [compojure "1.5.2"]
                 [ring/ring-core "1.6.0-RC2"]
                 [yesql "0.5.3"]])
