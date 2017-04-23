(ns clj3manchess.engine.state
  (:require [schema.core :as s]
            #?(:clj [clojure.spec :as sc]
               :cljs [cljs.spec :as sc])
            [clj3manchess.engine.color :as c]
            [clj3manchess.engine.board :as b]
            [clj3manchess.engine.castling :as ca]
            [clj3manchess.engine.pos :as p :refer [rank file]]
            [clojure.set :as set]))

(def MoatsState
  "Present in set if not yet bridged.
  Standard way of reffering to moats is by color which is next from these surrounding moats"
  #{c/Color})
(sc/def ::set-of-colors (sc/coll-of ::c/color :kind set? :min-count 0 :max-count 3 :distinct true :into #{}))
(sc/def ::moats ::set-of-colors)
(sc/def ::moves-next ::c/color)
(sc/def ::halfmoveclock (sc/and int? #(not (< % 0))))
(sc/def ::fullmovenumber (sc/and int? #(not (< % 0))))
(sc/def ::alive ::set-of-colors)

(def moats-state-on-newgame #{:white :gray :black})

(def EnPassant {(s/optional-key :prev) (s/maybe p/File)
                (s/optional-key :last) (s/maybe p/File)})
(sc/def ::prev (sc/nilable ::p/file))
(sc/def ::last (sc/nilable ::p/file))
(sc/def ::en-passant (sc/keys :req-un [::prev ::last]))
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
