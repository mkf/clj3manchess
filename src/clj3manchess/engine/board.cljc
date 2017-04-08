(ns clj3manchess.engine.board
  (:require [clojure.spec :as s]
            [clj3manchess.engine.vectors :as vec]))

(s/def ::figtype #{:pawn :rook :knight :bishop :queen :king})

(def tvec {:pawn ::vec/pawnvec
           :rook ::vec/axisvec
           :knight ::vec/knightvec
           :bishop ::vec/diagvec
           :queen ::vec/contvec
           :king ::vec/kingvec})

(s/def ::piece
  (s/keys :req-un [::figtype ::color]))

(s/def ::pawncenter boolean?)

(s/def ::figpawn (s/and ::piece (s/keys :req-un [::pawncenter])))
(s/def ::fig (s/or ::piece ::figpawn))

(s/def ::arrayboardrank (s/coll-of ::fig :kind vector? :count 24 :distinct false))
(s/def ::arrayboard (s/coll-of ::arrayboardrank :kind vector? :count 6 :distinct false))
