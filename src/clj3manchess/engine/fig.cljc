(ns clj3manchess.engine.fig
  (:require [schema.core :as s]
            [clj3manchess.engine.color :as c :refer [Color]]))

(def figtypeset #{:pawn :rook :knight :bishop :queen :king})
(def figtypeset-sanspawn (remove #{:pawn} figtypeset))
(def FigType (apply s/enum figtypeset))
(def FigTypeNotPawn (apply s/enum figtypeset-sanspawn))

(def Piece {(s/required-key :type) FigType
            (s/required-key :color) Color})

(def FigNotPawn {(s/required-key :type) FigTypeNotPawn
                 (s/required-key :color) Color})
(def Pawn {(s/required-key :type) (s/enum :pawn)
           (s/required-key :color) Color
           (s/required-key :crossedCenter) s/Bool})
(def Fig (s/either FigNotPawn Pawn))
