(ns clj3manchess.engine.state
  (:require [schema.core :as s]
            [clj3manchess.engine.color :as c]
            [clj3manchess.engine.board :as b]
            [clj3manchess.engine.castling :as ca]
            [clj3manchess.engine.pos :as p :refer [rank file]]
            [clojure.set :as set]))

(def MoatsState
  "Present in set if not yet bridged.
  Standard way of reffering to moats is by color which is next from these surrounding moats"
  #{c/Color})

(def moats-state-on-newgame #{:white :gray :black})

(def EnPassant {(s/optional-key :prev) (s/maybe p/File)
                (s/optional-key :last) (s/maybe p/File)})
(s/defn match-ep :- (s/maybe (s/enum :prev :last))
  [ep :- EnPassant, where :- p/Pos]
  (when (#{2 3} (rank where))
    (let [last (:last ep)
          prev (:prev ep)]
      (case (file where)
        last :last
        prev :prev))))
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
(defonce newgame {:board :clj3manchess.engine.board/newgame
                  :moats moats-state-on-newgame
                  :moves-next :white
                  :castling (set/join (map (fn [x] {:color x}) c/colors)
                                      (map (fn [x] {:type x}) #{:queenside :kingside}))
                  :en-passant {}
                  :halfmoveclock 0 :fullmovenumber 0
                  :alive (set c/colors)})
