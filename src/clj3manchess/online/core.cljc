(ns clj3manchess.online.core
  (:require [clj3manchess.engine.board :as b]
            [clj3manchess.engine.color :as c]
            [clj3manchess.engine.state :as st]
            [schema.core :as s]))
(def StateWithID (assoc st/State (s/required-key :id) s/Int))
