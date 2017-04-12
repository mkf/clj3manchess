(ns clj3manchess.engine.castling
  (:require [schema.core :as s]
            [clj3manchess.engine.color :as col :refer [Color]]))

(def CastlingType (s/enum :queenside :kingside))

(def CastlingPossibility {(s/required-key :color) Color
                          (s/required-key :type) CastlingType})

;;(def ColorCastlingPossibilities {CastlingType})

;;(def CastlingPossibilities {Color ColorCastlingPossibilities})
(def CastlingPossibilities #{CastlingPossibility})
