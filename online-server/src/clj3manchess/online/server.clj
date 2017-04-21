(ns clj3manchess.online.server
  (:require ;;[ring.middleware.params :refer [wrap-params]]
            ;;[compojure.core :refer [defroutes ANY context GET POST]]
            [compojure.api.sweet :refer :all]
            [schema.core :as s]
            [clj3manchess.engine.state :as st]
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
(defapi app
  (context "/api" []
           :tags ["api"]
           (GET "/move/:id" [id]
                (ok (d/get-just-move-by-id (intpars id))))
           (GET "/game/:id" [id]
                 (ok (d/get-gameplay-by-id (intpars id))))
           (GET "/state/:id" [id]
                :return StateWithID
                (ok (d/get-state-by-id (intpars id))))))

(def handler app)

(def init d/create-tables)
