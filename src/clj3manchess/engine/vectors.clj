(ns clj3manchess.engine.vectors
  (:use [clj3manchess.engine.pos] :reload-all
        [clojure.spec :as s]
        [clojure.set :as set])
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

;(s/def ::zerovec #{:zerovec})
(s/def ::abs (s/and integer? pos?))
(s/def ::inward boolean?)
(s/def ::plusFile boolean?)
(s/def ::centeronecloser boolean?)
(s/def ::pawnlongjumpvec #{:pawnlongjumpvec})
(s/def ::knightvec (s/keys :req-un [::plusFile ::inward ::centeronecloser (not ::abs)]))
(s/def ::filevec (s/keys :req-un [::plusFile (not ::inward) (not ::centeronecloser)] :opt-un [::abs]))
(s/def ::rankvec (s/keys :req-un [::inward (not ::plusFile) (not ::centeronecloser)] :opt-un [::abs]))
(s/def ::diagvec (s/keys :req-un [::plusFile ::inward (not ::centeronecloser)] :opt-un [::abs]))
(s/def ::axisvec (s/or ::filevec ::rankvec))
(s/def ::continuousvec (s/and (s/or ::axisvec ::diagvec) ::pawnpromvec))
(s/def ::multipliedvec (s/and (s/keys :req-un [::abs])) ::continuousvec)
(s/def ::kingvec (s/and ::continuousvec (not ::multipliedvec)))
(s/def ::pawnvec (s/or (s/and (s/or ::diagvec ::rankvec) (not ::multipliedvec) (s/keys :opt-un [::prom])) ::pawnlongjumpvec))
(s/def ::pawnpromvec (s/and ::pawnvec (s/keys :req-un [::prom])))
(s/def ::prom integer?)
(s/def ::castling #{:queenside :kingside})
(s/def ::castlingvec (s/keys :req-un [::castling]))


;; :zerovec or maybe just not use it
;; {:inward true :plusFile true :centeronecloser true}
;; {:castling :queenside}
;; {:inward true :plusFile true}
;; {:inward true :plusFile true :prom :queen}
;; {:inward false :abs 4}
;; {:plusFile true :abs 13}
;; {:inward true :plusFile true :abs 3}
;; :pawnlongjumpvec

(defn sgn [n] (cond (< n 0) -1 :else 1))
(defn abs [n] (cond (< n 0) (- n) :else n))
(def kfm 4)
(def castlingFileDiff {:queenside -2 :kingside 2})
(def castlingEmpties {:queenside '(3,2,1) :kingside '(5,6)})

(defn units [vec fromRank] (cond (contains? vec :abs)
                                      (cond
                                        (= (into set (keys vec)) #{:plusFile :abs}) (repeat (:abs vec) (dissoc vec :abs)
                                        (= (into set (keys vec)) #{:inward :abs}) (let
                                                                                      [tRank (cond (:inward vec) (:abs vec)
                                                                                                   :else (- (:abs vec)))]
                                                                                    (cond (and (:inward vec) (> (+ tRank fromRank) 5))
                                                                                          (concat
                                                                                           (repeat (inc (- 5 fromRank)) {:inward true})
                                                                                           (repeat (- (:abs vec) (inc (- 5 fromRank)))
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

(defn addvec [vec from] (cond
                               (contains? vec :centeronecloser) (cond
                                                                  (not (and (contains? vec :inward) (contains? vec :plusFile)))
                                                                  (throw (IllegalArgumentException.))
                                                                  (and
                                                                   (or
                                                                    (and (:centeronecloser vec) (>= (rank from) 4))
                                                                    (= (rank from) 5))
                                                                   (:inward vec))
                                                                  (cond (:centeronecloser vec) (Pos.
                                                                                                (- (+ 5 4) (rank from))
                                                                                                (mod (+
                                                                                                      (file from)
                                                                                                      (cond (:plusFile vec) 1 :else -1)
                                                                                                      12) 24))
                                                                        :else (Pos. 5
                                                                                    (mod (+
                                                                                          (file from)
                                                                                          (cond (:plusFile vec) 2 :else -2)
                                                                                          12) 24)))
                                                                  :else (Pos. (+
                                                                               (rank from)
                                                                               (cond (:inward vec) 1 :else -2)
                                                                               (cond (:centeronecloser vec) 1 :else 0))
                                                                              (mod (+
                                                                                    (file from)
                                                                                    (*
                                                                                     (cond
                                                                                       (not (= (:centeronecloser vec) (:inward vec)))
                                                                                       2 :else 1)
                                                                                     (cond (:plusFile vec) 1 :else -1))))))
                               (contains? vec :castling) (case (rank from) 0
                                                               (case (mod (file from) 8) kfm
                                                                     (Pos. 0
                                                                           (+ (file from) (castlingFileDiff (:castling vec))))))
                               (= vec :pawnlongjumpvec) (case (rank from) 1 (Pos. 3 (file from)))
                               (and (contains? vec :inward) (contains? vec :plusFile))
                               (let [abs (cond (contains? vec :abs) (:abs vec) :else 1)]
                                 (cond (not (:inward vec))
                                       (let [toRank (- (rank from) abs)]
                                         (cond (< toRank 0)
                                               (throw (IllegalArgumentException.))
                                               :else (Pos. toRank
                                                           (cond (:plusFile vec)
                                                                 (mod (+ (file from) abs) 24)
                                                                 :else
                                                                 (mod (- (file from) abs) 24)))))
                                       :else (let [fromPlusAbs (+ (rank from) abs)]
                                               (cond (< fromPlusAbs 5) (Pos. fromPlusAbs
                                                                             (mod ((cond (:plusFile vec) + :else -) (file from) abs) 24))
                                                     :else (let [further (- fromPlusAbs 6)
                                                                 howMuchHere (- 5 (rank from))
                                                                 fileAfterDirect (mod ((cond (:plusFile vec) + :else -)
                                                                                       (file from) howMuchHere) 24)]
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
                                                         (cond (:inward vec) (let [toRankDir (+ (rank from) abs)]
                                                                               (cond (> toRankDir 5)
                                                                                     (Pos. (- 11 toRankDir)
                                                                                           (mod (+ (file from) 12) 24))
                                                                                     :else (Pos. toRankDir (file from))))
                                                               :else (Pos. (- (rank from) abs) (file from))))
                               (contains? vec :plusFile) (let [abs (cond (contains? vec :abs) (:abs vec) :else 1)]
                                                           (Pos. (rank from) (mod ((cond (:plusFile vec) + :else -)
                                                                                    (file from) abs) 24)))
                               :else (throw (IllegalArgumentException.))))

(defn tfmapset [keyword] #{{keyword true} {keyword false}})

(defn creek (from vec) (and (< (rank from) 3)
                               (or (and (:plusFile vec) (= (mod (file from) 8) 7))
                                   (and (not (:plusFile vec)) (= mod (file from) 8) 0))))

(def vecft {
            ::axisvec (fn [from to] (set/union (set/select (complement nil?) #{((::rankvec vecft) from to)})
                                               ((:filevec vecft) from to)))
            ::castlingvec (fn [from to] (cond (and (= (rank from) 0 (rank to)) (= (mod (file from) 8) kfm))
                                              (case (mod (file to) 8)
                                                2 {:castling :queenside}
                                                6 {:castling :kingside}
                                                :default nil)))
            ::continuousvec (fn [from to] (set/union ((::axisvec vecft) from to)
                                                     ((::diagvec vecft) from to)))
            ::pawnwalkvec (fn [from to] (first (filter #(= to (addvec % from)) (tfmapset :inward))))
            ::pawnlongjumpvec (fn [from to] (cond (and (= (rank from) 1) (= (rank to) 3) (= (file from) (file to))) :pawnlongjump))
            ::pawncapvec (fn [from to] (first (filter #(and (not (creek from %)) (= to (addvec % from)))
                                        (set/join (tfmapset :inward) (tfmapset :plusFile)))))
            ::pawnvec (fn [from to] (first (filter (complement nil?)
                                                   '((::pawnwalkvec vecft) (::pawnlongjumpvec vecft) (::pawncapvec vecft)))))
            ::rankvec (fn [from to] (cond (sameOrAdjacent from to) (let [t (if (sameFile from to)
                                                                             (- (rank to) (rank from))
                                                                             (- 11 (+ (rank from) (rank to))))
                                                                         inward (> 0 t)
                                                                         abs (if inward t (- t))]
                                                                     (cond (= 1 abs) {:inward inward}
                                                                           (not (= 0 abs)) {:inward inward :abs abs}))))
            ::filevec (fn [from to] (filter (complement nil?) (sort-by :abs )))
                })
