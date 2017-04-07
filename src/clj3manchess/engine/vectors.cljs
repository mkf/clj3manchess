(ns clj3manchess.engine.vectors
  (:require [clojure.spec :as s] [clojure.set :as set]
            [clj3manchess.engine.pos :as p :refer [rank file color-segm pos-on-segm same-file same-rank
                                                   file-dist same-or-opposite-file opposite-file]]))

(defn one-if-nil-else-input [input] (if (nil? input) 1 input))

(defn abs [vec] {:pre [(s/valid? ::contvec vec)] :post [(s/valid? integer? %)]} (one-if-nil-else-input (:abs vec)))

(s/def ::abs (s/and integer? pos? #(< % 24)))
(s/def ::inward boolean?)
(s/def ::plusfile boolean?)
(s/def ::centeronecloser boolean?)
(s/def ::pawnlongjumpvec #{:pawnlongjumpvec})
(s/def ::knightvec (s/and (s/keys :req-un [::plusfile ::inward ::centeronecloser]) #(not (contains? % :abs))))
(s/def ::filevec (s/and (s/keys :req-un [::plusfile] :opt-un [::abs])
                        #(not (let [arg %] (or (contains? arg :inward) (contains? arg :centeronecloser))))))
(s/def ::rankvec (s/and (s/keys :req-un [::inward] :opt-un [::abs])
                        #(not (let [arg %] (or (contains? arg :plusfile) (contains? arg :centeronecloser))))
                        #(<= (abs %) 11)))
(s/def ::diagvec (s/and (s/keys :req-un [::plusfile ::inward] :opt-un [::abs])
                        #(not (contains? % :centeronecloser))
                        #(<= (abs %) 11)))
(s/def ::axisvec (s/or ::filevec ::rankvec))
(s/def ::contvec (s/and (s/or ::axisvec ::diagvec) ::pawnpromvec))
(s/fdef abs
        :args (s/cat :vec ::contvec)
        :ret ::abs)

(s/def ::multiplicablevec (s/and ::contvec #(not (contains? % :prom))))
(s/def ::multipliedvec (s/and ::multiplicablevec (s/keys :req-un [::abs]) #(> (:abs %) 1)))
(s/def ::kingvec (s/and ::contvec #(not (contains? % :abs))))
(s/def ::pawnvec (s/or (s/and (s/or ::diagvec ::rankvec)
                              #(not (contains? % :abs))
                              (s/keys :opt-un [::prom]))
                       ::pawnlongjumpvec))
(s/def ::pawnpromvec (s/and ::pawnvec (s/keys :req-un [::prom])))
(s/def ::prom integer?)
(s/def ::castling #{:queenside :kingside})
(s/def ::castlingvec (s/keys :req-un [::castling]))

(defn sgn [n] {:pre [(s/valid? number? n)]} (cond (< n 0) -1 :else 1))
(s/fdef sgn
        :args (s/cat :n number?)
        :ret #{-1 1})

(def kfm 4)
(def castlingFileDiff {:queenside -2 :kingside 2})
(def castlingEmpties {:queenside '(3,2,1) :kingside '(5,6)})


(defn thru-center-cont-vec?
  [inward abs from-rank] {:pre [(s/valid? boolean? inward) (s/valid? ::abs abs) (s/valid? ::rank from-rank)]}
  (and (not (nil? inward))
       (let [abs (one-if-nil-else-input abs)]
         (and inward (> (+ abs from-rank) 5))))
  [vec from-rank] {:pre [(s/valid? ::contvec vec) (s/valid? ::rank from-rank)]}
  (thru-center-rank-vec? (:inward vec) (:abs vec) from-rank))
(s/fdef thru-center-cont-vec?
        :args (s/cat :vecparam (s/alt :inwardabs (s/cat :inward boolean? :abs ::abs)
                                      :vec ::contvec)
                     :from-rank ::rank)
        :ret boolean?)

(defn ranks-inward-to-pass-center [from-rank] {:pre [(s/valid? ::rank from-rank)]} (inc (- 5 from-rank)))
(s/fdef ranks-inward-to-pass-center
        :args (s/cat :from-rank ::rank)
        :ret (s/and pos-int? #(<= % 6)))

(def one-inward {:inward true})
(def one-outward {:inward false})

(defn units-rank-vec
  [vec from-rank] {:pre [(s/valid? ::rankvec vec) (s/valid? ::rank from-rank)]} (units-rank-vec (:inward vec) (:abs vec) from-rank)
  [inward abs from-rank] {:pre [(s/valid? boolean? inward) (s/valid? ::abs abs) (s/valid? ::rank from-rank)]}
  (let [abs (one-if-nil-else-input abs)]
                           (if (thru-center-cont-vec? inward abs from-rank)
                           (concat (repeat (ranks-inward-to-pass-center from-rank) one-inward)
                                   (repeat (- abs (ranks-inward-to-pass-center from-rank)) one-outward))
                           (repeat abs {:inward inward}))))
(s/fdef units-rank-vec
        :args (s/cat :vecparam (s/alt :inwardabs (s/cat :inward boolean? :abs ::abs)
                                      :vec ::rankvec)
                     :from-rank ::rank)
        :ret (s/coll-of (s/and ::rankvec ::kingvec) :min-count 1))

(defn units-file-vec
  [vec] {:pre [(s/valid? ::filevec vec)]} (units-file-vec (:abs vec) (:plusfile vec))
  [abs plusfile] {:pre [(s/valid? ::abs abs) (s/valid? boolean? plusfile)]} (cond (not (nil? plusfile))
                       (repeat (one-if-nil-else-input abs) {:plusfile plusfile})))
(s/fdef units-file-vec
        :args (s/alt :plusfileabs (s/cat :plusfile boolean? :abs ::abs)
                     :vec ::filevec)
        :ret (s/coll-of (s/and ::rankvec ::kingvec) :min-count 1))

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
                                                                  (cond (:centeronecloser vec) [
                                                                                                (- (+ 5 4) (rank from))
                                                                                                (mod (+
                                                                                                      (file from)
                                                                                                      (cond (:plusfile vec) 1 :else -1)
                                                                                                      12) 24)]
                                                                        :else [5
                                                                                    (mod (+
                                                                                          (file from)
                                                                                          (cond (:plusfile vec) 2 :else -2)
                                                                                          12) 24)])
                                                                  :else [(+
                                                                               (rank from)
                                                                               (cond (:inward vec) 1 :else -2)
                                                                               (cond (:centeronecloser vec) 1 :else 0))
                                                                              (mod (+
                                                                                    (file from)
                                                                                    (*
                                                                                     (cond
                                                                                       (not (= (:centeronecloser vec) (:inward vec)))
                                                                                       2 :else 1)
                                                                                     (cond (:plusfile vec) 1 :else -1))))])
                               (contains? vec :castling) (case (rank from) 0
                                                               (case (mod (file from) 8) kfm
                                                                     [0
                                                                           (+ (file from) (castlingFileDiff (:castling vec)))]))
                               (= vec :pawnlongjumpvec) (case (rank from) 1 [3 (file from)])
                               (and (contains? vec :inward) (contains? vec :plusfile))
                               (let [abs (cond (contains? vec :abs) (:abs vec) :else 1)]
                                 (cond (not (:inward vec))
                                       (let [toRank (- (rank from) abs)]
                                         (cond (< toRank 0)
                                               (throw (IllegalArgumentException.))
                                               :else [toRank
                                                           (cond (:plusfile vec)
                                                                 (mod (+ (file from) abs) 24)
                                                                 :else
                                                                 (mod (- (file from) abs) 24))]))
                                       :else (let [from-plus-abs (+ (rank from) abs)]
                                               (cond (< from-plus-abs 5) [from-plus-abs
                                                                             (mod ((cond (:plusfile vec) + :else -) (file from) abs) 24)]
                                                     :else (let [further (- from-plus-abs 6)
                                                                 howMuchHere (- 5 (rank from))
                                                                 fileAfterDirect (mod ((cond (:plusfile vec) + :else -)
                                                                                       (file from) howMuchHere) 24)]
                                                             (cond (= further -1) [5 fileAfterDirect]
                                                                   :else (cond (> further 5) (throw (IllegalArgumentException.))
                                                                               :else (let [rankAfter (- 5 further)
                                                                                           solelyThruCenterFile (mod (+
                                                                                                                      fileAfterDirect
                                                                                                                      (cond (:plusfile vec)
                                                                                                                            -10 :else 10))
                                                                                                                     24)]
                                                                                       (cond (= further 0) [5 solelyThruCenterFile]
                                                                                             :else [rankAfter
                                                                                                         (mod ((cond (not (:plusfile vec))
                                                                                                                     + :else -)
                                                                                                               solelyThruCenterFile
                                                                                                               further) 24)])))))))))
                               (contains? vec :inward) (let [abs (cond (contains? vec :abs) (:abs vec) :else 1)]
                                                         (cond (:inward vec) (let [toRankDir (+ (rank from) abs)]
                                                                               (cond (> toRankDir 5)
                                                                                     [(- 11 toRankDir)
                                                                                           (mod (+ (file from) 12) 24)]
                                                                                     :else [toRankDir (file from)]))
                                                               :else [(- (rank from) abs) (file from)]))
                               (contains? vec :plusfile) (let [abs (cond (contains? vec :abs) (:abs vec) :else 1)]
                                                           [(rank from) (mod ((cond (:plusfile vec) + :else -)
                                                                                    (file from) abs) 24)])
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
