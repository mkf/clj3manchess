(ns clj3manchess.online.server
  (:require [liberator.core :refer [resource defresource]]
             [ring.middleware.params :refer [wrap-params]]
             [compojure.core :refer [defroutes ANY context GET POST]]
             [clj3manchess.online.server.mysql :as d]))

(defroutes app
  (ANY "/" [] (resource))
  (context "/api" []
           (defroutes api-routes
             (ANY "/" [] (resource)))))

(def handler (-> app wrap-params))

(def init d/create-tables)
