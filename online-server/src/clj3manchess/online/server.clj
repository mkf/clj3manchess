(ns clj3manchess.online.server
  (:require [liberator.core :refer [resource defresource]]
             [ring.middleware.params :refer [wrap-params]]
             [compojure.core :refer [defroutes ANY context GET POST]]
             [clj3manchess.online.server.mysql :as d]))

(defresource state-resource [id]
  :allowed-methods [:get]
  :exists? (fn [_]
             (when-let [e (d/get-state-by-id id)] {::state e}))
  :available-media-types ["application/edn"]
  :handle-ok #(prn-str (::state %)))

(defroutes app
  (ANY "/" [] (resource))
  (context "/api" []
           (defroutes api-routes
             (ANY ["/state/:id{[0-9]+}"] [id] (state-resource id))
             (ANY "/" [] (resource)))))

(def handler (-> app wrap-params))

(def init d/create-tables)
