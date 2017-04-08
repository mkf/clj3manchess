(ns clj3manchess.engine.board
  (:require [schema.core :as s]
            [clj3manchess.engine.fig :as f :refer [FigType Fig]]
            [clj3manchess.engine.vectors :as vec]))

;(s/def ::arrayboardrank (s/coll-of ::fig :kind vector? :count 24 :distinct false))
;(s/def ::arrayboard (s/coll-of ::arrayboardrank :kind vector? :count 6 :distinct false))

(def Square (s/maybe Fig))
