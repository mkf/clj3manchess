(ns clj3manchess.engine.move
  (:require [schema.core :as s]
            [clj3manchess.engine.state :as st]
            [clj3manchess.engine.vectors :as v]
            [clj3manchess.engine.pos :as p]
            [clj3manchess.engine.fig :as f]
            [clj3manchess.engine.board :as b]
            [clj3manchess.engine.color :as c]
            [clojure.set :as se]
            [clj3manchess.engine.castling :as ca]))

(def VecMove {(s/required-key :vec) v/Vec
              (s/required-key :from) p/Pos
              (s/required-key :before) st/State})

(def PawnCapVecMove {(s/required-key :vec) v/PawnCapVec
                     (s/required-key :from) p/Pos
                     (s/required-key :before) st/State})

(def FromTo {(s/required-key :from) p/Pos
             (s/required-key :to) p/Pos})

(def Desc {(s/required-key :from) p/Pos
           (s/required-key :to) p/Pos
           (s/optional-key :prom) f/PromFigType})

(def DescMove {(s/required-key :from) p/Pos
               (s/required-key :to) p/Pos
               (s/optional-key :prom) f/PromFigType
               (s/required-key :before) st/State})

(def Move (s/either DescMove VecMove))

(s/defn to :- p/Pos [move :- Move]
  (if (contains? move :to) (:to move) (v/bv-to (dissoc move :before))))

(s/defn get-bef-sq :- b/Square [move :- Move, where :- p/Pos]
  (b/getb (:board (:before move)) where))

(s/defn is-the-fig-we-cap-not-ours :- s/Bool [m :- Move]
  (not= (:moves-next (:before m))
        (:color (get-bef-sq m (to m)))))

(s/defn can-we-en-passant :- s/Bool [m :- PawnCapVecMove]
  (let [to-sq (get-bef-sq m (to m))]
    (and (nil? to-sq)
         (= :pawn (:type (get-bef-sq m (assoc (to m) 0 3))))
         (st/matching-ep (:en-passant (:before m)) (to m)
                         (:moves-next (:before m))
                         (get-bef-sq m (assoc (to m) 0 3)))))) ;(st/match-ep (:en-passant (:before m)) (to m))]
                       ;(case match
                       ;  :last (= (c/prev-col (:moves-next (:before m)))
                       ;           (:color (get-bef-sq m (assoc (to m) 0 3))))
                       ;  :prev (= (c/next-col (:moves-next (:before m)))
                       ;           (:color (get-bef-sq m (assoc (to m) 0 3))))
                       ;  false)


(def initially-checked-impossibilities #{:nothing-to-move-here
                                         :not-your-move
                                         :cannot-en-passant
                                         :capturing-own-piece
                                         :not-all-empties
                                         :no-castling-possibility
                                         :capturing-thru-moats
                                         :passing-unbridged-moats
                                         :no-promotion
                                         :wrong-pawn-direction})
(def later-checked-impossibilities #{:we-in-check
                                     :castling-over-check
                                     :initiating-check-thru-moats})
(def impossibilities (se/union initially-checked-impossibilities
                               later-checked-impossibilities))

(def InitiallyCheckedImpossibility (apply s/enum initially-checked-impossibilities))
(def LaterCheckedImpossibility (apply s/enum later-checked-impossibilities))
(def Impossibility (apply s/enum impossibilities))

(s/defn initial-impossibilities-check :- (s/maybe InitiallyCheckedImpossibility)
  [m :- VecMove]
  (cond
    (nil? (get-bef-sq m (:from m))) :nothing-to-move-here
    (and (= (:type (get-bef-sq m (:from m))) :pawn)
         (contains? (:vec m) :plusfile)
         (not (can-we-en-passant m))) :cannot-en-passant
    (= (:color (get-bef-sq m (:from m)))
       (:color (get-bef-sq m (to m)))) :capturing-own-piece
    (and (contains? (:vec m) :abs)
         (not (b/check-empties (:board (:before m))
                               (v/empties-cont-vec (:vec m) (:from m))))) :not-all-empties
    (and (contains? (:vec m) :castling)
         ((:castling (:before m))
           {:type (:castling (:vec m)) :color (:moves-next (:before m))}))
    :no-castling-possibility
    (and (contains? (:vec m) :castling)
         (not (b/check-empties
                (:board (:before m))
                (->> v/castling-empties
                     (map (partial + (* 8 (c/segm (:color (:moves-next (:before m)))))))
                     (map (fn [x] [0 x])))))) :not-all-empties
    false :capturing-thru-moats
    false :passing-unbridged-moats
    ;(= (:type (get-bef-sq m (:from m))) :pawn) (if-not (:inward (:vec m))
    ;                                             (if-not (:crossedCenter (get-bef-sq m (:from m)))
    ;                                               :wrong-pawn-direction
    ;                                               (if )))
    (and (= (:type (get-bef-sq m (:from m))) :pawn)
         (= (:inward (:vec m)) (:crossedCenter (get-bef-sq m (:from m))))) :wrong-pawn-direction
    (and (= (:type (get-bef-sq m (:from m))) :pawn)
         (= (p/rank (to m)) 5)
         (not (f/promfigtypes (:prom (:vec m))))) :no-promotion
    (not= (:moves-next (:before m))
          (:color (get-bef-sq m (:from m)))) :not-your-move))

(s/defn board-after-pawn-cap :- b/Board [bef :- b/Board,
                                         from :- p/Pos, to :- p/Pos,
                                         ep :- st/EnPassant]
  (let [we (b/getb bef from)]
    (if (st/matching-ep ep to (:color we) (:color (b/getb bef [3 (p/file to)])))
      (-> bef
          (b/mov from to)
          (b/clr [3 (p/file to)]))
      (b/mov bef from to))))

(s/defn board-after-pawn-prom :- b/Board [bef :- b/Board,
                                          from :- p/Pos, to :- p/Pos,
                                          prom :- f/PromFigType]
  (let [color (:color (b/getb bef from))]
    (-> bef
        (b/clr from)
        (b/put to {:type prom :color color}))))

(s/defn board-after-castling :- b/Board [bef :- b/Board,
                                         color :- c/Color, castling :- ca/CastlingType]
  (let [color-segm*8 (* 8 (c/segm color))
        kfm-on-segm (+ p/kfm color-segm*8)
        castling-sgnf (castling v/castling-file-diff-sgnf)
        old-rook-pos (+ color-segm*8 (castling v/castling-bef-rook-pos))
        to-empty (castling v/castling-empties)
        to-empty (map (partial + color-segm*8) to-empty)
        to-empty (conj to-empty kfm-on-segm old-rook-pos)
        new-king-pos (+ kfm-on-segm (castling-sgnf 2))
        new-rook-pos (+ kfm-on-segm (castling-sgnf 1))
        emptied (loop [cur-res bef, left-to to-empty]
                  (if (empty? left-to) cur-res
                                       (recur (b/clr cur-res [0 (first left-to)])
                                              (rest left-to))))]
    (-> emptied
        (b/put [0 new-rook-pos] {:type :rook :color color})
        (b/put [0 new-king-pos] {:type :king :color color})))
  )