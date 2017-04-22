(defproject clj3manchess/online-server "0.3.0-SNAPSHOT"
  :plugins [[lein-ring "0.8.11"]]
  :ring {:handler clj3manchess.online.server/handler
         :init clj3manchess.online.server/init}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [clj3manchess/engine "0.3.0-SNAPSHOT"]
                 [clj3manchess/online-core "0.3.0-SNAPSHOT"]
                 [liberator "0.14.1"] ;;not anymore
                 [compojure "1.5.2"]
                 [metosin/compojure-api "1.1.10"]
                 [ring/ring-core "1.6.0-RC2"]
                 [com.layerware/hugsql "0.4.7"]
                 [ring-cors "0.1.10"]
                 [mysql/mysql-connector-java "5.1.39"]
                 [prismatic/schema "1.1.5"]]
  :profiles {:uberjar {:aot :all}
             :production {}
             :dev {:dependencies [[ring/ring-mock "0.3.0"]
                                  [ring/ring-devel "1.6.0-RC2"]
                                  [javax.servlet/javax.servlet-api "3.1.0"]]}})
