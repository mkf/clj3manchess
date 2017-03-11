(ns clj3manchess.engine.vectors
  (:use [clj3manchess.engine.pos] :reload-all)
  (:import [clj3manchess.engine.pos Pos]))

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
(def kfm 4)
(def castlingFileDiff {:queenside -2 :kingside 2})
(def castlingEmpties {:queenside '(3,2,1) :kingside '(5,6)})

(defn units [vec fromRank] (cond (contains? vec :abs)
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

(defn addVec [vec from] (cond
                               (contains? vec :centerOneCloser) (cond
                                                                  (not (and (contains? vec :inward) (contains? vec :plusFile)))
                                                                  (throw (IllegalArgumentException.))
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
                               (contains? vec :castling) (case (.rank from) 0
                                                               (case (mod (.file from) 8) kfm
                                                                     (Pos. 0
                                                                           (+ (.file from) (castlingFileDiff (:castling vec))))))
                               (= vec :pawnlongjump) (case (.rank from) 1 (Pos. 3 (.file from)))
                               (and (contains? vec :inward) (contains? vec :plusFile))
                               (let [abs (cond (contains? vec :abs) (:abs vec) :else 1)]
                                 (cond (not (:inward vec))
                                       (let [toRank (- (.rank from) abs)]
                                         (cond (< toRank 0)
                                               (throw (IllegalArgumentException.))
                                               :else (Pos. toRank
                                                           (cond (:plusFile vec)
                                                                 (mod (+ (.file from) abs) 24)
                                                                 :else
                                                                 (mod (- (.file from) abs) 24)))))
                                       :else (let [fromPlusAbs (+ (.rank from) abs)]
                                               (cond (< fromPlusAbs 5) (Pos. fromPlusAbs
                                                                             (mod ((cond (:plusFile vec) + :else -) (.file from) abs) 24))
                                                     :else (let [further (- fromPlusAbs 6)
                                                                 howMuchHere (- 5 (.rank from))
                                                                 fileAfterDirect (mod ((cond (:plusFile vec) + :else -)
                                                                                       (.file from) howMuchHere) 24)]
                                                             (cond (= further -1) (Pos. 5 fileAfterDirect)
                                                                   :else (cond (> further 5) (throw (IllegalArgumentException.))
                                                                               :else (let [rankAfter (- 5 further)
                                                                                           solelyThruCenterFile (mod (+
                                                                                                                      fileAfterDirect
                                                                                                                      (cond (:plusFile vec)
                                                                                                                            -10 :else 10))
                                                                                                                     24)]
                                                                                       (cond (= further 0) (Pos. 5 solelyThruCenterFile)
                                                                                             :else (Pos. rankAfter
                                                                                                         (mod ((cond (not (:plusFile vec))
                                                                                                                     + :else -)
                                                                                                               solelyThruCenterFile
                                                                                                               further) 24)))))))))))
                               (contains? vec :inward) (let [abs (cond (contains? vec :abs) (:abs vec) :else 1)]
                                                         (cond (:inward vec) (let [toRankDir (+ (.rank from) abs)]
                                                                               (cond (> toRankDir 5)
                                                                                     (Pos. (- 11 toRankDir)
                                                                                           (mod (+ (.file from) 12) 24))
                                                                                     :else (Pos. toRankDir (.file from))))
                                                               :else (Pos. (- (.rank from) abs) (.file from))))
                               (contains? vec :plusFile) (let [abs (cond (contains? vec :abs) (:abs vec) :else 1)]
                                                           (Pos. (.rank from) (mod ((cond (:plusFile vec) + :else -)
                                                                                    (.file from) abs) 24)))
                               :else (throw (IllegalArgumentException.))))
