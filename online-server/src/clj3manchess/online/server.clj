(ns clj3manchess.online.server
  (:require ;;[ring.middleware.params :refer [wrap-params]]
            ;;[compojure.core :refer [defroutes ANY context GET POST]]
   [compojure.api.sweet :refer :all]
   [schema.core :as s]
   [clj3manchess.engine.state :as st]
   [clj3manchess.engine.move :as m :refer [Desc]]
   [ring.util.http-response :refer :all]
   [ring.middleware.cors :refer [wrap-cors]]
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
  (if-let [beforegameobj (d/get-gameplay-by-id beforegame)]
    (m/after-of-afters {:from from :to to :prom prom :before beforegameobj})))
(defn aftergame [ftp]
  (if-let [asta (afterstate ftp)]
    (d/insert-gameplay! asta)))
(defapi app
  {:swagger
   {:ui   "/api-docs"
    :spec "/swagger.json"
    :data {:info {:title       "TMC API"
                  :description "Some docs"}
           :tags [{:name "api", :description "main API"}]}}}
  (context "/api" []
    :tags ["api"]
    (GET "/game/:id/after" [id]
      (let [id (intpars id)]
        (if-let [res (d/get-just-moves-by-before id)]
          (ok res) (not-found {:id id}))))
    (POST "/game/:id" [id]
      :body [ftp Desc]
      :return {(s/required-key :id) s/Int}
      (let [id  (intpars id)
            ftp (assoc ftp :beforegame id)]
        (if-let [agame (aftergame ftp)]
          (ok {:id (d/insert-just-move!
                    (assoc ftp :aftergame agame))})
          (not-found {:id id}))))
    (GET "/move/:id" [id]
      (let [id (intpars id)]
        (if-let [res (d/get-just-move-by-id id)]
          (ok res) (not-found {:id id}))))
    (GET "/game/:id" [id]
      (let [id (intpars id)]
        (if-let [res (d/get-gameplay-by-id  id)]
          (ok res) (not-found {:id id}))))
    (GET "/newgame" []
      (if-let [id (d/insert-gameplay! st/newgame)]
        (ok {:id id}) (internal-server-error {:id nil})))
    (GET "/state/:id" [id]
      :return StateWithID
      (let [id (intpars id)]
        (if-let [res (d/get-state-by-id  id)]
          (ok res) (not-found {:id id}))))))

(def handler (wrap-cors app
                        :access-control-allow-origin "*"
                        :access-control-allow-methods [:get :post]))

(defn init [] (do (d/create-tables)))
