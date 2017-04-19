(ns clj3manchess.online.server
  (:require [liberator.core :refer [resource defresource]]
             [ring.middleware.params :refer [wrap-params]]
             [compojure.core :refer [defroutes ANY]]))

(defroutes app
  (ANY "/" [] (resource)))

(def handler (-> app wrap-params))
