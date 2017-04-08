(ns clj3manchess.engine.board
  (:require [schema.core :as s]
            [clj3manchess.engine.fig :as f :refer [FigType]]
            [clj3manchess.engine.vectors :as vec]))

(def tvec {:pawn ::vec/pawnvec
           :rook ::vec/axisvec
           :knight ::vec/knightvec
           :bishop ::vec/diagvec
           :queen ::vec/contvec
           :king ::vec/kingvec})

;(s/def ::arrayboardrank (s/coll-of ::fig :kind vector? :count 24 :distinct false))
;(s/def ::arrayboard (s/coll-of ::arrayboardrank :kind vector? :count 6 :distinct false))
