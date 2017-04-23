(ns clj3manchess.engine.board
  (:require [schema.core :as s]
            #?(:clj [clojure.spec :as sc]
               :cljs [cljs.spec :as sc])
            [clj3manchess.engine.fig :as f :refer [FigType Fig]]
            [clj3manchess.engine.vectors :as vec]
            [clj3manchess.engine.pos :as p :refer [Pos rank file]]
            [clojure.string :as stri]
            [clj3manchess.engine.color :as c]))

;(s/def ::arrayboardrank (s/coll-of ::fig :kind vector? :count 24 :distinct false))
;(s/def ::arrayboard (s/coll-of ::arrayboardrank :kind vector? :count 6 :distinct false))

(def Square (s/maybe Fig))
(sc/def ::sq (sc/nilable ::f/fig))

(def ArrayBoard [[Square]])
(sc/def ::arr (sc/coll-of (sc/coll-of ::sq :king vector? :max-count 24 :distinct false :into [])
                             :kind vector? :max-count 6 :distinct false :into []))
(def MapBoard {Pos Fig})
(sc/def ::map (sc/map-of ::p/pos ::f/fig :kind map? :max-count 144 :into {}))
(def NewGameBoard (s/eq ::newgame))
(sc/def ::new #{::newgame})
(def AbsBoard (s/either ArrayBoard MapBoard NewGameBoard))
(sc/def ::abs (sc/or :arr ::arr :map ::map :new ::new))
(def Diff {Pos Square})
(sc/def ::diffs (sc/map-of ::p/pos ::sq :kind map? :max-count 144 :into {}))
(def DiffBoard [(s/one AbsBoard "base") (s/one Diff "diff")])
(sc/def ::diffed (sc/tuple ::abs ::diffs))
(def BoardOfVectorStructure (s/either DiffBoard ArrayBoard))
(sc/def ::vec (sc/or :arr ::arr :diffed ::diffed))
(s/defn diff-board? :- s/Bool [b :- BoardOfVectorStructure] (map? (second b)))
(def Board (s/either AbsBoard DiffBoard))
(sc/def ::any (sc/or :abs ::abs :diffed ::diffed))

(s/defn get-from-array-board :- Square [b :- ArrayBoard, pos :- Pos]
  (-> b
      (get (rank pos))
      (get (file pos))))
(sc/fdef get-from-array-board
         :args (sc/cat :b ::arr :pos ::p/pos)
         :ret ::sq)

(s/defn get-from-map-board :- Square [b :- MapBoard, pos :- Pos] (get b pos))
(sc/fdef get-from-map-board
         :args (sc/cat :b ::map :pos ::p/pos)
         :ret ::sq)

(def newgame-zero-rank-segm [:rook :knight :bishop :queen :king :bishop :knight :rook])

(s/defn get-from-newgame-board :- Square
  ([b :- NewGameBoard, pos :- Pos] (if (not= ::newgame b) ::sc/invalid
                                       (get-from-newgame-board pos)))
  ([pos :- Pos] (case (rank pos)
                  0 {:type  (get newgame-zero-rank-segm (mod (file pos) 8))
                     :color (p/color-segm pos)}
                  1 {:type :pawn :color (p/color-segm pos) :crossed-center false}
                  nil)))
(sc/fdef get-from-newgame-board
         :args (sc/or :both (sc/cat :b ::new :pos ::p/pos)
                      :pos (sc/cat :pos ::p/pos))
         :ret ::sq)

(sc/def ::dispatch-abs (sc/or :arr ::arr :map ::map :new ::new))
(s/defn get-from-abs-board :- Square [b :- AbsBoard, pos :- Pos]
  (cond
    (vector? b) (get-from-array-board b pos)
    (map? b) (get-from-map-board b pos)
    (= ::newgame b) (get-from-newgame-board pos)))
(sc/fdef get-from-abs-board :args (sc/cat :b ::abs :pos ::p/pos) :ret ::sq)

(s/defn get-from-diff-board :- Square [b :- DiffBoard, pos :- Pos]
  (if (contains? (second b) pos)
    (get (second b) pos)
    (get-from-abs-board (first b) pos)))
(sc/fdef get-from-diff-board :args (sc/cat :b ::diffed :pos ::p/pos) :ret ::sq)

(s/defn getb :- Square [b :- Board, pos :- Pos]
  (cond
    (vector? b) (if (diff-board? b) (get-from-diff-board b pos) (get-from-array-board b pos))
    :else (get-from-abs-board b pos)))
(sc/fdef getb :args (sc/cat :b ::any :pos ::p/pos) :ret ::sq)

(s/defn where-are-figs :- [Pos] [b :- Board, fig :- Fig]
  (filter (comp (partial = fig)
                (partial getb b)) p/all-pos))
(sc/fdef where-are-figs :args (sc/cat :b ::any :fig ::f/fig)
         :ret (sc/coll-of ::p/pos :kind sequential? :max-count 8 :distinct true)
         :fn #(for [x (:ret %)] (= (getb (-> % :args :b) x)
                                   (-> % :args :fig))))
(s/defn where-are-figs-of-type :- [Pos] [b :- Board, typ :- FigType]
  (filter (comp (partial = typ)
                :type
                (partial getb b)) p/all-pos))
(sc/fdef where-are-figs-of-type :args (sc/cat :b ::any :typ ::f/type)
         :ret (sc/coll-of ::p/pos :kind sequential? :max-count 24 :distinct true)
         :fn #(for [x (:ret %)] (= (:type (getb (-> % :args :b) x))
                                   (-> % :args :typ))))
(s/defn where-are-kings :- {c/Color Pos} [b :- Board]
  (into {} (map (fn [x] [(:color (getb b x)) x]) (where-are-figs-of-type b :king))))
(sc/fdef where-are-kings :args (sc/cat :b ::any)
         :ret (sc/coll-of ::p/pos :kind sequential? :max-count 3 :distinct true)
         :fn #(for [[color x] (:ret %)] (= (getb (-> % :args :b) x)
                                           {:type :king :color color})))
(s/defn where-are-figs-of-color :- [Pos] [b :- Board, col :- c/Color]
  (filter (comp (partial = col)
                :color
                (partial getb b)) p/all-pos))
(sc/fdef where-are-figs-of-color :args (sc/cat :b ::any :col ::c/color)
         :ret (sc/coll-of ::p/pos :kind sequential? :max-count 16 :distinct true)
         :fn #(for [x (:ret %)] (= (:color (getb (-> % :args :b) x))
                                   (-> % :args :col))))
(s/defn where-is-fig :- (s/maybe Pos) [b :- Board, fig :- Fig] (first (where-are-figs b fig)))
(s/defn where-is-king :- (s/maybe Pos) [b :- Board, col :- c/Color] (where-is-fig b {:type :king :color col}))

(s/defn put-onto-array-board :- ArrayBoard [b :- ArrayBoard, pos :- Pos, what :- Square]
  (let [b (if (>= (count b) (rank pos)) b (into b (repeat (- (rank pos) (count b)) [])))
        the-rank (get b (rank pos))
        b (if (>= (count the-rank) (file pos))
            b (assoc b (rank pos)
                     (into the-rank
                           (repeat (- (file pos) (count the-rank))
                                   []))))]
    (assoc-in b pos what)))

(s/defn put-onto-map-board :- MapBoard [b :- MapBoard, pos :- Pos, what :- Square]
  (if (nil? what) (dissoc b pos) (assoc b pos what)))

(s/defn put-onto-diff-board :- DiffBoard [b :- DiffBoard, pos :- Pos, what :- Square]
  (if (= (getb (first b) pos) what) [(first b) (dissoc (second b) pos)]
      [(first b) (assoc (second b) pos what)]))

(s/defn put-onto-newgame-board :- DiffBoard [pos :- Pos, what :- Square]
  (put-onto-diff-board [::newgame {}] pos what))

(s/defn put :- Board [b :- Board, pos :- Pos, what :- Square]
  (cond
    (vector? b) (if (diff-board? b) (put-onto-diff-board b pos what)
                    (put-onto-array-board b pos what))
    (map? b) (put-onto-map-board b pos what)
    (= b ::newgame) (put-onto-newgame-board pos what)))

(s/defn clr :- Board [b :- Board, pos :- Pos]
  (put b pos nil))

(s/defn mov :- Board [b :- Board, from :- Pos, to :- Pos]
  (let [what (getb b from)]
    (-> b
        (clr from)
        (put to what))))

(s/defn fill-map-board :- MapBoard [b :- Board]
  (->> p/all-pos
       (map (fn [x] [x (getb b x)]))
       (apply concat)
       (apply hash-map)))

;; (s/defn fill-array-board :- ArrayBoard [b :- Board]
;;   (->> (range 6)
;;        (map (fn [ra] (->> (range 24)
;;                           (map #(getb b [ra %]))
;;                           (into []))) )
;;        ;;(into [])
;;        (map f/figstr)
;;        (into [])))

(s/defn fill-array-board :- ArrayBoard [b :- Board]
  (map (fn [ra] (map #(getb b [ra %]) (range 24))) (range 6)))

(s/defn string-of-rank :- s/Str
  [r :- [Square]] (str "[" (stri/join " " (map f/figstr r)) "]"))
(s/defn string-of-arrayboard :- s/Str
  [b :- ArrayBoard] (str "[" (stri/join " \n " (map string-of-rank (reverse b))) "]"))

(s/defn check-empties :- s/Bool
  [b :- Board , w :- [Pos]] (every? true? (map nil? (map #(getb b %) w))))

(s/defn squares-array :- [Square] [b :- Board]
  (apply concat (fill-array-board b)))
(s/defn sqint-array :- [s/Int] [b :- Board]
  (map f/figtoint (squares-array b)))
;;(def ngsiaa (sqint-array ::newgame))
;;ngsiaa
(s/defn board-from-sqint :- ArrayBoard [sr :- [s/Int]]
  (->> sr
       (map f/inttofig)
       (partition 24)
       (map vec)
       vec))
;;(board-from-sqint ngsiaa)
