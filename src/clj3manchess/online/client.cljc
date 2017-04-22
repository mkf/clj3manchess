(ns clj3manchess.online.client
  (:require [schema.core :as s]
            [clj3manchess.online.core :refer [StateWithID]]
            [clj3manchess.engine.move :as m :refer [Desc]]
            [slingshot.slingshot :refer [try+]]
            [clj-http.client :as cl]))

(def ^:dynamic *base-url* "http://localhost:3000")
(def ^:dynamic *after-base-url* "/api")
(defn url [& following] (apply (partial str *base-url* *after-base-url*) following))

(s/defn get-just-moves-by-before [id :- s/Int] (try+
                                                (:body (cl/get (url "/game/" id "/after") {:accept :edn :as :clojure}))
                                                (catch [:status 404] _ nil)))
(s/defn post-move :- {(s/required-key :id) s/Int} [id :- s/Int, ftp :- Desc]
  (try+
   (:body (cl/post (url "/game/" id) {:accept :edn :as :clojure :content-type :clojure :body (prn-str ftp)
                                      :throw-entire-message? true}))
   (catch [:status 404] _ nil)))
(s/defn get-just-move-by-id [id :- s/Int] (try+
                                           (:body (cl/get (url "/move/" id)
                                                    {:accept :edn :as :clojure :throw-entire-message? true}))
                                                (catch [:status 404] _ nil)))
(s/defn get-gameplay-by-id [id :- s/Int] (try+ (:body (cl/get (url "/game/" id) {:accept :edn :as :clojure}))
                                               (catch [:status 404] _ nil)))
(s/defn newgame :- {(s/required-key :id) s/Int} [] (try+ (:body (cl/get (url "/newgame") {:accept :edn :as :clojure}))
                                                         (catch [:status 404] _ nil)
                                                         (catch [:status 500] _ nil)))
(s/defn get-state-by-id :- StateWithID [id :- s/Int] (try+ (:body (cl/get (url "/state/" id) {:accept :edn :as :clojure}))
                                                           (catch [:status 404] _ nil)))
