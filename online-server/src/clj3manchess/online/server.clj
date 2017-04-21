(ns clj3manchess.online.server
  (:require ;;[ring.middleware.params :refer [wrap-params]]
            ;;[compojure.core :refer [defroutes ANY context GET POST]]
   [compojure.api.sweet :refer :all]
   [schema.core :as s]
   [clj3manchess.engine.state :as st]
   [clj3manchess.engine.move :as m :refer [Desc]]
   [ring.util.http-response :refer :all]
   [clj3manchess.online.server.mysql :as d]
   [clj3manchess.online.core :refer [StateWithID]]))

;; (defresource state-resource [id]
;;   :allowed-methods [:get]
;;   :exists? (fn [_]
;;              (when-let [e (d/get-state-by-id id)] {::state e}))
;;   :available-media-types ["application/edn"]
;;   :handle-ok #(prn-str (::state %)))
;; (defroutes app
;;   (context "/api" []
;;            (defroutes api-routes
;;              (ANY ["/state/:id{[0-9]+}"] [id] (state-resource id)))))
(defn intpars [id] (if-not (string? id) id (Integer/parseInt id)))
(defn afterstate [{:keys [from to beforegame prom] :as ftp}]
  (let [beforegameobj (d/get-gameplay-by-id beforegame)
        _ (println (prn-str beforegameobj))]
    (m/after-of-afters {:from from :to to :prom prom :before beforegameobj})))
(defn aftergame [ftp]
  (d/insert-gameplay! (afterstate ftp)))
(defapi app
  (context "/api" []
    :tags ["api"]
    (GET "/game/:id/after" [id]
      (ok (d/get-just-moves-by-before (intpars id))))
    (POST "/game/:id" [id]
      :body [ftp Desc]
      :return {(s/required-key :id) s/Int}
      (ok (let [id (intpars id)]
            {:id (d/insert-just-move!
                  (let [ftp (assoc ftp :beforegame id)
                        agame (aftergame ftp)]
                    (assoc ftp :aftergame (aftergame ftp))))})))
    (GET "/move/:id" [id]
      (ok (d/get-just-move-by-id (intpars id))))
    (GET "/game/:id" [id]
      (ok (d/get-gameplay-by-id (intpars id))))
    (GET "/newgame" []
      (ok {:id (d/insert-gameplay! st/newgame)}))
    (GET "/state/:id" [id]
      :return StateWithID
      (ok (d/get-state-by-id (intpars id))))))

(def handler app)

(defn init [] (do (d/create-tables)))
