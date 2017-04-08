(ns clj3manchess.engine.castling
  (:require [schema.core :as s]
            [clj3manchess.engine.color :as col :refer [Color]]))

(def CastlingType (s/enum :queenside :kingside))

(def ColorCastlingPossibilities {CastlingType s/Bool})

(def CastlingPossibilities {Color ColorCastlingPossibilities})
