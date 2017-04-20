(ns clj3manchess.online.server.mysql
  (:require [hugsql.core :as hugsql]))

(hugsql/def-db-fns "clj3manchess/online/server/sql/everything.sql")
(hugsql/def-sqlvec-fns "clj3manchess/online/server/sql/everything.sql")

(defn get-env-var [name] (System/getenv name))

(def db {:subprotocol "mysql"
         :subname (get-env-var "TMC_DB_URL")
         :user (get-env-var "TMC_DB_USER")
         :password (get-env-var "TMC_DB_PASS")})
