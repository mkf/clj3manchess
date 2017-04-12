(ns clj3manchess.engine.state
  (:require [schema.core :as s]
            [clj3manchess.engine.color :as c]
            [clj3manchess.engine.board :as b]
            [clj3manchess.engine.castling :as ca]
            [clj3manchess.engine.pos :as p :refer [rank file]]))

(def MoatsState [(s/one s/Bool "w") (s/one s/Bool "g") (s/one s/Bool "b")])
(s/defn moat-betw-prev-and-this :- s/Bool
  "Standard way of referring to moats is by color which is next from these surrounding moats"
  [moats-state :- MoatsState, and-what :- c/Color]
  (get moats-state (case and-what
                     :white 0 :gray 1 :black 2)))
(def EnPassant {(s/optional-key :prev) p/File
                (s/optional-key :last) p/File})
(s/defn match-ep :- (s/maybe (s/enum :prev :last))
  [ep :- EnPassant, where :- p/Pos]
  (when (#{2 3} (rank where))
    (case (file where)
      (:last ep) :last
      (:prev ep) :prev)))
(s/defn matching-ep :- s/Bool
  [ep :- EnPassant, where :- p/Pos, color-of-ours :- c/Color, color-of-captured :- c/Color]
  (let [match (match-ep ep where)]
    (case match
      :last (= (c/prev-col color-of-ours) color-of-captured)
      :prev (= (c/next-col color-of-ours) color-of-captured)
      false)))
(def Alive #{c/Color})
(def State {(s/required-key :board)          b/Board
            (s/required-key :moats)          MoatsState
            (s/required-key :moves-next)     c/Color
            (s/required-key :castling)       ca/CastlingPossibilities
            (s/required-key :en-passant)     EnPassant
            (s/required-key :halfmoveclock)  s/Int
            (s/required-key :fullmovenumber) s/Int
            (s/required-key :alive)          Alive})
