(ns clj3manchess.online.server.mysql
  (:require [hugsql.core :as hugsql]
            [clj3manchess.engine.board :as b]
            [clj3manchess.engine.color :as c]
            [clj3manchess.engine.castling :as ca]
            [clj3manchess.engine.state :as st]
            [clj3manchess.engine.fig :as f]
            [clj3manchess.engine.pos :as p :refer [rank file]]
            [clojure.string :as str]
            [clj-time.coerce :as tc]))

(hugsql/def-db-fns "clj3manchess/online/server/sql/everything.sql")
(hugsql/def-sqlvec-fns "clj3manchess/online/server/sql/everything.sql")

(defn get-env-var ([name else] (if-let [sth (System/getenv name)] sth else))
  ([name] (get-env-var name nil)))

(def db {:subprotocol "mysql"
         :subname (get-env-var "TMC_DB_URL" "//127.0.0.1:3306/archiet")
         :user (get-env-var "TMC_DB_USER" "archiet")
         :password (get-env-var "TMC_DB_PASS" "hugtest")})
(defn create-tables []
  (do (create-st-table db)
      (create-gp-table db)
      (create-mv-table db)))
(defn set-of-colors-to-stringset [colors] (->> colors
                                               (map c/colchar)
                                               (str/join \,)))
(defn stringset-to-set-of-colors [string] (->> string
                                               (partition-by #{\,})
                                               (filter #(not= \, (first %)))
                                               (map first)
                                               (map c/charcol)
                                               set))
(defn get-state-by-id [id] (when-let [{:keys [board movesnext moats castling fullmovenumber halfmoveclock
                                              alive enpassant_prev enpassant_last]
                                       :as fromdb} (get-st-by-id db {:id id})]
                             {:board (b/board-from-sqint (vec board))
                              :moves-next (c/charcol (first movesnext))
                              :id id
                              :moats (stringset-to-set-of-colors moats)
                              :castling (set (ca/charsetcastposset castling))
                              :fullmovenumber fullmovenumber
                              :halfmoveclock halfmoveclock
                              :en-passant {:prev enpassant_prev
                                           :last enpassant_last}
                              :alive (stringset-to-set-of-colors alive)}))
(defn state-to-st [{:keys [board moves-next moats castling fullmovenumber halfmoveclock alive en-passant]
                    :as state}]
  {:board (byte-array 144 (b/sqint-array board))
   :movesnext (str (c/colchar moves-next))
   :moats (set-of-colors-to-stringset moats)
   :castling (ca/castpossetstr castling)
   :fullmovenumber fullmovenumber
   :halfmoveclock halfmoveclock
   :alive (set-of-colors-to-stringset alive)
   :enpassant_prev (:prev en-passant)
   :enpassant_last (:last en-passant)})
(defn insert-state! [state]
  (:generated_key (into {}
                        [(insert-new-st!
                          db
                          (state-to-st state))])))
(defn get-gameplay-by-id [id]
  (when-let [{:keys [state created]} (get-just-gp-by-id db {:id id})]
    (assoc (get-state-by-id state) :gp_id id :created (tc/from-sql-time created))))
(defn insert-gameplay! [state]
  (:generated_key
   (into {}
         [(insert-new-gp! db {:state (insert-state! state)})])))
(defn insert-just-move! [{:keys [from to prom beforegame aftergame]}]
  (:generated_key
   (into {}
         [(insert-new-mv! db {:fromto (byte-array 4 [(rank from) (file from) (rank to) (file to)])
                              :prom (when-not (nil? prom) (str (f/figtypechars prom)))
                              :beforegame beforegame
                              :aftergame aftergame})])))
(defn mv-to-move [{:keys [fromto prom beforegame aftergame] :as mv}]
  (when mv (let [fromto (vec fromto)
                 [from to] (map vec (partition 2 fromto))
                 promotion (when-not (nil? prom) (f/figtypebychar prom))]
             {:from from :to to :promotion promotion :beforegame beforegame :aftergame aftergame})))
(defn get-just-move-by-id [id]
  (mv-to-move (get-just-mv-by-id db {:id id})))
(defn get-just-moves-by-before [id]
  (keep mv-to-move (get-just-mvs-by-before db {:id id})))
