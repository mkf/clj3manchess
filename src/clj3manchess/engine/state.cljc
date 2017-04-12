(ns clj3manchess.engine.state
  (:require [schema.core :as s]
            [clj3manchess.engine.color :as c]
            [clj3manchess.engine.board :as b]
            [clj3manchess.engine.castling :as ca]
            [clj3manchess.engine.pos :as p]))

(def MoatsState [(s/one s/Bool "w") (s/one s/Bool "g") (s/one s/Bool "b")])
(def EnPassant [(s/one p/File "prev") (s/one p/File "last")])
(def Alive #{c/Color})
(def State {(s/required-key :board)          b/Board
            (s/required-key :moats)          MoatsState
            (s/required-key :moves-next)     c/Color
            (s/required-key :castling)       ca/CastlingPossibilities
            (s/required-key :en-passant)     EnPassant
            (s/required-key :halfmoveclock)  s/Int
            (s/required-key :fullmovenumber) s/Int
            (s/required-key :alive)          Alive})

