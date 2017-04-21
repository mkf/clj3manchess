(ns clj3manchess.online.server
  (:require ;;[ring.middleware.params :refer [wrap-params]]
            ;;[compojure.core :refer [defroutes ANY context GET POST]]
            [compojure.api.sweet :refer :all]
            [schema.core :as s]
            [clj3manchess.engine.state :as st]
            [ring.util.http-response :refer :all]
            [clj3manchess.online.server.mysql :as d]))

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
(def StateWithID (assoc st/State (s/required-key :id) s/Int))
(defapi app
  (context "/api" []
           :tags ["api"]
           (GET "/state/:id" [id]
                :return {:result StateWithID}
                (ok {:result (d/get-state-by-id id)}))))

(def handler app)

(def init d/create-tables)
