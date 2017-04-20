(defproject clj3manchess.online.server "0.1.0-SNAPSHOT"
  :plugins [[lein-ring "0.8.11"]]
  :ring {:handler clj3manchess.online.server/handler
         :init clj3manchess.online.server/init}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [clj3manchess "0.2.0-SNAPSHOT"]
                 [liberator "0.14.1"]
                 [compojure "1.5.2"]
                 [ring/ring-core "1.6.0-RC2"]
                 [com.layerware/hugsql "0.4.7"]
                 [mysql/mysql-connector-java "5.1.39"]
                 [prismatic/schema "1.1.5"]]
  :profiles {:uberjar {:aot :all}
             :production {}
             :dev {:dependencies [[ring/ring-mock "0.3.0"]
                                  [ring/ring-devel "1.6.0-RC2"]]}})
