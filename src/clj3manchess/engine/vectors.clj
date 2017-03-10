(ns archiet.clj3manchess.engine.vectors)

(import archiet.clj3manchess.engine.pos)

;; (defprotocol Vector
;;   (rank [this])
;;   (file [this])
;;   (addTo [this ^Pos from])
;;   (units [this ^int fromRank])
;;   (emptiesFrom [this ^Pos from])
;;   (moats [this ^Pos from])
;; )
;; (defrecord ZeroVector [] Vector (file [this] 0) (rank [this] 0) (addTo [this ^Pos from] from)
;;            (units [this ^int fromRank] '()) (emptiesFrom [this ^Pos from] '()) (moats [this ^Pos from] '()))
;; (defprotocol JumpVector Vector)
;; (defprotocol KingVector Vector)
;; (defprotocol PawnVector Vector
;;              (reqpc [this])
;;              (reqProm [this]))
;; (defprotocol ContinuousVector)

;; :zerovec or maybe just not use it
;; {:inward true :plusFile true :centerOneCloser true}
;; {:castling :queenside}
;; {:inward true :plusFile true}
;; {:inward true :plusFile true :prom :queen}
;; {:inward false :abs 4}
;; {:plusFile true :abs 13}
;; {:inward true :plusFile true :abs 3}
;; :pawnlongjump

(defn sgn [n] (cond (< n 0) -1 :else 1))
(defn abs [n] (cond (< n 0) (- n) :else n))

(defn units [vec ^int fromRank] (cond (contains? vec :abs)
                                      (cond
                                        (= (into set (keys vec)) #{:plusFile :abs}) (repeat (abs (:file vec)) {:file (sgn (:rank vec))})
                                        (= (into set (keys vec)) #{:inward :abs}) (let
                                                                                   [tRank (cond (:inward vec) (:abs vec) :else (- (:abs vec)))]
                                                                                    (cond (> (+ tRank fromRank) 5)
                                                                                          (concat
                                                                                           (repeat (inc (- 5 fromRank)) {:inward true})
                                                                                           (repeat (- (:rank vec) (inc (- 5 fromRank)))
                                                                                                   {:inward false}))
                                                                                          :else (repeat (:abs vec) (dissoc vec :abs))))

                                        (and (contains? vec :plusFile) (contains? vec :inward))
                                        (cond (not (:inward vec)) (repeat (:abs vec) (dissoc vec :abs))
                                              :else (let [fromPlusAbs (+ fromRank (:abs vec))]
                                                      (cond (> fromPlusAbs 6) (concat
                                                                               (repeat (inc (- 5 fromRank)) (dissoc vec :abs))
                                                                               (repeat (- (:abs vec) (inc (- 5 fromRank)))
                                                                                       (assoc (dissoc vec :abs)
                                                                                              :inward false
                                                                                              :plusFile (not (:plusFile vec)))))
                                                            :else (repeat (:abs vec) (dissoc vec :abs)))))
                                        :else (throw (IllegalArgumentException.))) ;; maybe return vec?
                                      :else vec))

(defn addVec [vec ^Pos from] (cond
                              (contains? vec :centerOneCloser) (cond
                                                                 (and
                                                                  (or
                                                                   (and (:centerOneCloser vec) (>= (.rank from) 4))
                                                                   (= (.rank from) 5))
                                                                  (:inward vec))
                                                                 (cond (:centerOneCloser vec) (Pos.
                                                                                               (- (+ 5 4) (.rank from))
                                                                                               (mod (+
                                                                                                     (.file from)
                                                                                                     (cond (:plusFile vec) 1 :else -1)
                                                                                                     12) 24))
                                                                       :else (Pos. 5
                                                                                   (mod (+
                                                                                         (.file from)
                                                                                         (cond (:plusFile vec) 2 :else -2)
                                                                                         12) 24)))
                                                                 :else (Pos. (+
                                                                              (.rank from)
                                                                              (cond (:inward vec) 1 :else -2)
                                                                              (cond (:centerOneCloser vec) 1 :else 0))
                                                                             (mod (+
                                                                                   (.file from)
                                                                                   (*
                                                                                    (cond
                                                                                      (not (= (:centerOneCloser vec) (:inward vec)))
                                                                                      2 :else 1)
                                                                                    (cond (:plusFile vec) 1 :else -1))))))
                              ))
