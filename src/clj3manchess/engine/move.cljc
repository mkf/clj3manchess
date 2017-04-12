(ns clj3manchess.engine.move
  (:require [schema.core :as s]
            [clj3manchess.engine.state :as st]
            [clj3manchess.engine.vectors :as v]
            [clj3manchess.engine.pos :as p]
            [clj3manchess.engine.fig :as f]
            [clj3manchess.engine.board :as b]
            [clj3manchess.engine.color :as c]))

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
    (and (nil? to-sq) (let [match (st/match-ep (:en-passant (:before m)) to-sq)]
                       (case match
                         :last (= (c/prev-col (:moves-next (:before m)))
                                  (:color (get-bef-sq m (assoc (to m) 0 2))))
                         :prev (= (c/next-col (:moves-next (:before m)))
                                  (:color (get-bef-sq m (assoc (to m) 0 2))))
                         false)))))
