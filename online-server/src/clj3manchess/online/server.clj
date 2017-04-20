(ns clj3manchess.online.server
  (:require [liberator.core :refer [resource defresource]]
             [ring.middleware.params :refer [wrap-params]]
             [compojure.core :refer [defroutes ANY context GET POST]]))

(defroutes app
  (ANY "/" [] (resource))
  (context "/api" []
           (defroutes api-routes
             (ANY "/" [] (resource)))))

(def handler (-> app wrap-params))

(defn init [] nil)
