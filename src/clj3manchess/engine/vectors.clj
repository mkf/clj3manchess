(ns clj3manchess.engine.vectors
  (:use [clj3manchess.engine.pos] :reload-all
        [clojure.spec :as s]
        [clojure.set :as set])
  (:import [clj3manchess.engine.pos Pos]))

;; (defprotocol Vector
;;   (rank [this])
;;   (file [this])
;;   (addTo [this ^Pos from])
;;   (units [this ^int from-rank])
;;   (emptiesFrom [this ^Pos from])
;;   (moats [this ^Pos from])
;; )
;; (defrecord ZeroVector [] Vector (file [this] 0) (rank [this] 0) (addTo [this ^Pos from] from)
;;            (units [this ^int from-rank] '()) (emptiesFrom [this ^Pos from] '()) (moats [this ^Pos from] '()))
;; (defprotocol JumpVector Vector)
;; (defprotocol KingVector Vector)
;; (defprotocol PawnVector Vector
;;              (reqpc [this])
;;              (reqProm [this]))
;; (defprotocol ContVector)

;(s/def ::zerovec #{:zerovec})
(s/def ::abs (s/and integer? pos?))
(s/def ::inward boolean?)
(s/def ::plusfile boolean?)
(s/def ::centeronecloser boolean?)
(s/def ::pawnlongjumpvec #{:pawnlongjumpvec})
(s/def ::knightvec (s/keys :req-un [::plusfile ::inward ::centeronecloser (not ::abs)]))
(s/def ::filevec (s/keys :req-un [::plusfile (not ::inward) (not ::centeronecloser)] :opt-un [::abs]))
(s/def ::rankvec (s/keys :req-un [::inward (not ::plusfile) (not ::centeronecloser)] :opt-un [::abs]))
(s/def ::diagvec (s/keys :req-un [::plusfile ::inward (not ::centeronecloser)] :opt-un [::abs]))
(s/def ::axisvec (s/or ::filevec ::rankvec))
(s/def ::contvec (s/and (s/or ::axisvec ::diagvec) ::pawnpromvec))
(s/def ::multipliedvec (s/and (s/keys :req-un [::abs])) ::contvec)
(s/def ::kingvec (s/and ::contvec (not ::multipliedvec)))
(s/def ::pawnvec (s/or (s/and (s/or ::diagvec ::rankvec) (not ::multipliedvec) (s/keys :opt-un [::prom])) ::pawnlongjumpvec))
(s/def ::pawnpromvec (s/and ::pawnvec (s/keys :req-un [::prom])))
(s/def ::prom integer?)
(s/def ::castling #{:queenside :kingside})
(s/def ::castlingvec (s/keys :req-un [::castling]))


;; :zerovec or maybe just not use it
;; {:inward true :plusfile true :centeronecloser true}
;; {:castling :queenside}
;; {:inward true :plusfile true}
;; {:inward true :plusfile true :prom :queen}
;; {:inward false :abs 4}
;; {:plusfile true :abs 13}
;; {:inward true :plusfile true :abs 3}
;; :pawnlongjumpvec

(defn sgn [n] (cond (< n 0) -1 :else 1))
(defn abs [n] (cond (< n 0) (- n) :else n))
(def kfm 4)
(def castlingFileDiff {:queenside -2 :kingside 2})
(def castlingEmpties {:queenside '(3,2,1) :kingside '(5,6)})

(defn one-if-nil-else-input [input] (if (nil? input) 1 input))

(defn thru-center-cont-vec?
  [inward abs from-rank] (and (not (nil? inward)) (let [abs (one-if-nil-else-input abs)] (and inward (> (+ abs from-rank) 5))))
  [vec from-rank] (thru-center-rank-vec? (:inward vec) (:abs vec) from-rank))

(defn ranks-inward-to-pass-center [from-rank] (inc (- 5 from-rank)))
(def one-inward {:inward true})
(def one-outward {:inward false})

(defn units-rank-vec
  [vec from-rank] (units-rank-vec (:inward vec) (:abs vec) from-rank)
  [inward abs from-rank] (let [abs (one-if-nil-else-input abs)]
                           (if (thru-center-cont-vec? inward abs from-rank)
                           (concat (repeat (ranks-inward-to-pass-center from-rank) one-inward)
                                   (repeat (- abs (ranks-inward-to-pass-center from-rank)) one-outward))
                           (repeat abs {:inward inward}))))

(defn units-file-vec
  [vec] (units-file-vec (:abs vec) (:plusfile vec))
  [abs plusfile] (cond (not (nil? plusfile)) (repeat (one-if-nil-else-input abs) {:plusfile plusfile})))

(defn units-diag-vec
  [vec from-rank] (units-diag-vec (:inward vec) (:plusfile vec) (:abs vec))
  [inward plusfile abs from-rank] (let [abs (one-if-nil-else-input abs)]
                                    (if (not inward) (repeat abs {:inward inward :plusfile plusfile})
                                        (if (thru-center-cont-vec? inward abs from-rank)
                                          (concat
                                           (repeat (ranks-inward-to-pass-center from-rank)
                                                   {:inward inward :plusfile plusfile})
                                           (repeat (- abs (ranks-inward-to-pass-center from-rank))
                                                   {:inward false :plusfile (not plusfile)}))
                                          (repeat abs {:inward inward :plusfile plusfile})))))


(defn units [vec from-rank] (cond (contains? vec :abs)
                                      (cond
                                        (= (into set (keys vec)) #{:plusfile :inward :abs}) (units-diag-vec vec from-rank)
                                        (= (into set (keys vec)) #{:plusfile :abs}) (units-file-vec vec)
                                        (= (into set (keys vec)) #{:inward :abs}) (units-rank-vec vec from-rank)
                                        :else (throw (IllegalArgumentException.))) ;; maybe return vec?
                                      :else '(vec))))

(defn addvec [vec from] (cond
                               (contains? vec :centeronecloser) (cond
                                                                  (not (and (contains? vec :inward) (contains? vec :plusfile)))
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
                                                                                                      (cond (:plusfile vec) 1 :else -1)
                                                                                                      12) 24))
                                                                        :else (Pos. 5
                                                                                    (mod (+
                                                                                          (file from)
                                                                                          (cond (:plusfile vec) 2 :else -2)
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
                                                                                     (cond (:plusfile vec) 1 :else -1))))))
                               (contains? vec :castling) (case (rank from) 0
                                                               (case (mod (file from) 8) kfm
                                                                     (Pos. 0
                                                                           (+ (file from) (castlingFileDiff (:castling vec))))))
                               (= vec :pawnlongjumpvec) (case (rank from) 1 (Pos. 3 (file from)))
                               (and (contains? vec :inward) (contains? vec :plusfile))
                               (let [abs (cond (contains? vec :abs) (:abs vec) :else 1)]
                                 (cond (not (:inward vec))
                                       (let [toRank (- (rank from) abs)]
                                         (cond (< toRank 0)
                                               (throw (IllegalArgumentException.))
                                               :else (Pos. toRank
                                                           (cond (:plusfile vec)
                                                                 (mod (+ (file from) abs) 24)
                                                                 :else
                                                                 (mod (- (file from) abs) 24)))))
                                       :else (let [from-plus-abs (+ (rank from) abs)]
                                               (cond (< from-plus-abs 5) (Pos. from-plus-abs
                                                                             (mod ((cond (:plusfile vec) + :else -) (file from) abs) 24))
                                                     :else (let [further (- from-plus-abs 6)
                                                                 howMuchHere (- 5 (rank from))
                                                                 fileAfterDirect (mod ((cond (:plusfile vec) + :else -)
                                                                                       (file from) howMuchHere) 24)]
                                                             (cond (= further -1) (Pos. 5 fileAfterDirect)
                                                                   :else (cond (> further 5) (throw (IllegalArgumentException.))
                                                                               :else (let [rankAfter (- 5 further)
                                                                                           solelyThruCenterFile (mod (+
                                                                                                                      fileAfterDirect
                                                                                                                      (cond (:plusfile vec)
                                                                                                                            -10 :else 10))
                                                                                                                     24)]
                                                                                       (cond (= further 0) (Pos. 5 solelyThruCenterFile)
                                                                                             :else (Pos. rankAfter
                                                                                                         (mod ((cond (not (:plusfile vec))
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
                               (contains? vec :plusfile) (let [abs (cond (contains? vec :abs) (:abs vec) :else 1)]
                                                           (Pos. (rank from) (mod ((cond (:plusfile vec) + :else -)
                                                                                    (file from) abs) 24)))
                               :else (throw (IllegalArgumentException.))))

(defn tfmapset [keyword] #{{keyword true} {keyword false}})

(defn creek (from vec) (and (< (rank from) 3)
                               (or (and (:plusfile vec) (= (mod (file from) 8) 7))
                                   (and (not (:plusfile vec)) (= mod (file from) 8) 0))))

(defn wrappedfilevec [t f wlong] (let [diff (- t f)
                                       sgnf (if (< diff 0) - +)]
                                   (if (= wlong (< 12 (sgnf diff))) diff (- diff (sgnf 24)))))

(def vecft {
            ::axisvec (fn [from to] (set/union (set/select (complement nil?) #{((::rankvec vecft) from to)})
                                               ((:filevec vecft) from to)))
            ::castlingvec (fn [from to] (cond (and (= (rank from) 0 (rank to)) (= (mod (file from) 8) kfm))
                                              (case (mod (file to) 8)
                                                2 {:castling :queenside}
                                                6 {:castling :kingside}
                                                :default nil)))
            ::contvec (fn [from to] (set/union ((::axisvec vecft) from to)
                                                     ((::diagvec vecft) from to)))
            ::pawnwalkvec (fn [from to] (first (filter #(= to (addvec % from)) (tfmapset :inward))))
            ::pawnlongjumpvec (fn [from to] (cond (and (= (rank from) 1) (= (rank to) 3) (= (file from) (file to))) :pawnlongjump))
            ::pawncapvec (fn [from to] (first (filter #(and (not (creek from %)) (= to (addvec % from)))
                                        (set/join (tfmapset :inward) (tfmapset :plusfile)))))
            ::pawnvec (fn [from to] (first (filter (complement nil?)
                                                   '((::pawnwalkvec vecft) (::pawnlongjumpvec vecft) (::pawncapvec vecft)))))
            ::rankvec (fn [from to] (cond (sameOrAdjacent from to) (let [t (if (sameFile from to)
                                                                             (- (rank to) (rank from))
                                                                             (- 11 (+ (rank from) (rank to))))
                                                                         inward (> 0 t)
                                                                         abs (if inward t (- t))]
                                                                     (cond (= 1 abs) {:inward inward}
                                                                           (not (= 0 abs)) {:inward inward :abs abs}))))
            ::filevec (fn [from to] (filter (complement nil?) (sort-by :abs (let [diff (- (file to) (file from))
                                                                                  plusfile (< 0 diff)
                                                                                  absdiff (if plusfile diff (- diff))]
                                                                              (cond (not (= 0 absdiff))
                                                                                    '({:plusfile plusfile :abs absdiff}
                                                                                      {:plusfile (not plusfile) :abs (- 24 absdiff)}))))))
            ::knightvec (fn [from to] (first (filter #(= to (addvec % from))
                                                     (set/join (tfmapset :inward) (tfmapset :plusfile) (tfmapset :centeronecloser)))))
            ::kingcontvec (fn [from to] (first (filter #(= to (addvec % from))
                                                   (set/union (tfmapset :inward) (tfmapset :plusfile)
                                                              (set/join (tfmapset :inward) (tfmapset :plusfile))))))
            ::kingvec (fn [from to] (first (filter (complement nil?) '((::castlingvec vecft) (::kingcontvec vecft)))))
            ::diagvec (fn [from to] (let [filediff (wrappedfilevec (file from) (file to))
                                          plusfile (> filediff 0)
                                          absfilediff (if plusfile filediff (- filediff))
                                          inwardshort (> (rank to) (rank from))
                                          absrankdiff (if inwardshort (- (rank to) (rank from)) (- (rank from) (rank to)))
                                          ser (cond (and (not (= 0 absrankdiff)) (= absfilediff absrankdiff))
                                                    {:abs absfilediff :inward inwardshort :plusfile plusfile})
                                          ranksum (+ (rank to) (rank from))
                                          ler (cond (and (not (= 0 absfilediff)) (= absfilediff ranksum))
                                                    {:abs (- (+ 5 5 1) ranksum) ;; (5-s)+1+(5-r)
                                                     :inward true :plusfile (not plusfile)})]
                                      (filter (complement nil?) '(ser ler))))
                })
