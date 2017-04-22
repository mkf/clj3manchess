(ns clj3manchess.online.client
  (:require [schema.core :as s]
            [clj3manchess.online.core :refer [StateWithID]]
            [clj3manchess.engine.move :as m :refer [Desc]]
            [kvlt.core :as kvlt]
            [promesa.core :as p]))

(def ^:dynamic *base-url* "http://localhost:8083")
(def ^:dynamic *after-base-url* "/api")
(defn url [& following] (apply (partial str *base-url* *after-base-url*) following))

(s/defn get-just-moves-by-before [id :- s/Int]
  (p/alet [{:keys [status body]} (p/await (kvlt/request! {:url (url "/game/" id "/after")
                                                          :accept :edn
                                                          :content-type :edn
                                                          :as :edn}))] (when (= status 200) body)))
(s/defn post-move ;;:- {(s/required-key :id) s/Int}
  [id :- s/Int, ftp :- Desc]
  (p/alet [{:keys [status body]} (p/await (kvlt/request! {:method :post
                                                          :url (url "/game/" id)
                                                          :accept :edn
                                                          :content-type :edn
                                                          :as :edn
                                                          :body ^:kvlt.body/edn ftp}))] (when (= status 200) body)))
(s/defn get-just-move-by-id [id :- s/Int]
  (p/alet [{:keys [status body]} (p/await (kvlt/request! {:url (url "/move/" id)
                                                          :accept :edn
                                                          :content-type :edn
                                                          :as :edn}))] (when (= status 200) body)))
(s/defn get-gameplay-by-id [id :- s/Int]
  (p/alet [{:keys [status body]} (p/await (kvlt/request! {:url (url "/game/" id)
                                                          :accept :edn
                                                          :content-type :edn
                                                          :as :edn}))] (when (= status 200) body)))
(s/defn newgame ;; :- {(s/required-key :id) s/Int}
  [] (p/alet [{:keys [status body]} (p/await (kvlt/request! {:url (url "/newgame")
                                                             :accept :edn
                                                             :content-type :edn
                                                             :as :edn}))] (when (= status 200) body)))
(s/defn get-state-by-id ;;:- StateWithID
  [id :- s/Int]
  (p/alet [{:keys [status body]} (p/await (kvlt/request! {:url    (url "/state/" id)
                                                          :accept :edn
                                                          :content-type :edn
                                                          :as     :edn}))] (when (= status 200) body)))
