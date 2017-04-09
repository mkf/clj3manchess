(ns clj3manchess.engine.board
  (:require [schema.core :as s]
            [clj3manchess.engine.fig :as f :refer [FigType Fig]]
            [clj3manchess.engine.vectors :as vec]
            [clj3manchess.engine.pos :as p :refer [Pos rank file]]))

;(s/def ::arrayboardrank (s/coll-of ::fig :kind vector? :count 24 :distinct false))
;(s/def ::arrayboard (s/coll-of ::arrayboardrank :kind vector? :count 6 :distinct false))

(def Square (s/maybe Fig))

(def ArrayBoard [[Square]])
(def MapBoard {Pos Fig})
(def NewGameBoard (s/eq ::newgame))
(def AbsBoard (s/either ArrayBoard MapBoard NewGameBoard))
(def Diff {Pos Square})
(def DiffBoard [(s/one AbsBoard "base") (s/one Diff "diff")])
(def BoardOfVectorStructure (s/either DiffBoard ArrayBoard))
(s/defn diff-board? :- s/Bool [b :- BoardOfVectorStructure] (map? (second b)))
(def Board (s/either AbsBoard DiffBoard))

(s/defn get-from-array-board :- Square [b :- ArrayBoard, pos :- Pos]
  (-> b
      (get (rank pos))
      (get (file pos))))

(s/defn get-from-map-board :- Square [b :- MapBoard, pos :- Pos] (get b pos))

(def newgame-zero-rank-segm [:rook :knight :bishop :queen :king :bishop :knight :rook])

(s/defn get-from-newgame-board :- Square ([b :- NewGameBoard, pos :- Pos] (get-from-newgame-board pos))
  ([pos :- Pos] (case (rank pos)
                  0 {:type (get newgame-zero-rank-segm (mod (file pos) 8))
                     :color (p/color-segm pos)}
                  1 {:type :pawn :color (p/color-segm pos) :crossedCenter false}
                  nil)))

(s/defn get-from-abs-board :- Square [b :- AbsBoard, pos :- Pos]
  (cond
    (vector? b) (get-from-array-board b pos)
    (map? b) (get-from-map-board b pos)
    (= ::newgame b) (get-from-newgame-board pos)))

(s/defn get-from-diff-board :- Square [b :- DiffBoard, pos :- Pos]
  (if (contains? (second b) pos)
    (get (second b) pos)
    (get-from-abs-board (first b) pos)))

(s/defn getb :- Square [b :- Board, pos :- Pos]
  (cond
    (vector? b) (if (diff-board? b) (get-from-diff-board b pos) (get-from-array-board b pos))
    :else (get-from-abs-board b pos)))

(s/defn put-onto-array-board :- ArrayBoard [b :- ArrayBoard, pos :- Pos, what :- Square]
  (let [b (if (>= (count b) (rank pos)) b (into b (repeat (- (rank pos) (count b)) [])))
        the-rank (get b (rank pos))
        b (if (>= (count the-rank) (file pos)) b (assoc b (rank pos) (into the-rank (repeat (- (file pos) (count the-rank)) []))))]
    (assoc-in b pos what)))

(s/defn put-onto-map-board :- MapBoard [b :- MapBoard, pos :- Pos, what :- Square]
  (if (nil? what) (dissoc b pos) (assoc b pos what)))

(s/defn put-onto-diff-board :- DiffBoard [b :- DiffBoard, pos :- Pos, what :- Square]
  (if (= (getb (first b) pos) what) [(first b) (dissoc (second b) pos)] [(first b) (assoc (second b) pos what)]))

(s/defn fill-map-board :- MapBoard [b :- Board]
  (->> p/all-pos
       (map (fn [x] [x (getb b x)]))
       (apply concat)
       (apply hash-map)))
