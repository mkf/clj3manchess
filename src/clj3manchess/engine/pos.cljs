(ns clj3manchess.engine.pos
  (:require
       [clj3manchess.engine.color :as col]
       [clojure.spec :as s]))

;(defrecord Pos [rank file])
(s/def ::rank (s/and integer? (comp neg?) #(<= % 5)))
(s/def ::file (s/and integer? (comp neg?) #(<= % 23)))
(s/def ::file-on-segm (s/and integer? (comp neg?) #(<= % 7)))
(s/def ::pos (s/tuple ::rank ::file))

(defn rank [pos] {:pre [(s/valid? ::pos pos)]} (first pos))
(s/fdef rank
        :args (s/cat :pos ::pos)
        :ret ::rank)

(defn file [pos] {:pre [(s/valid? ::pos pos)]} (last pos))
(s/fdef file
        :args (s/cat :pos ::pos)
        :ret ::file)

(defn color-segm [pos] {:pre [(s/valid? ::pos pos)]} (col/colors (quot (file pos) 8)))
(s/fdef color-segm
        :args (s/cat :pos ::pos)
        :ret ::color)

(defn pos-on-segm [color rank file-on-segm] {:pre [(s/valid? ::color color) (s/valid? ::rank rank) (s/valid? ::file-on-segm file-on-segm)]}
  [rank (+ file-on-segm
           (bit-shift-left (col/segm color) 3))])
(s/fdef pos-on-segm
        :args (s/cat :color ::color :rank ::rank :file-on-segm ::file-on-segm)
        :ret ::pos)

(defn same-rank [a b] {:pre [(s/valid? ::pos a) (s/valid? ::pos b)]} (= (rank a) (rank b)))
(s/fdef same-rank
        :args (s/cat :a ::pos :b ::pos)
        :ret boolean?)

(defn same-file [a b] {:pre [(s/valid? ::pos a) (s/valid? ::pos b)]} (= (file a) (file b)))
(s/fdef same-file
        :args (s/cat :a ::pos :b ::pos)
        :ret boolean?)

(defn file-dist [a b] {:pre [(s/valid? ::pos a) (s/valid? ::pos b)] :post [(s/valid? ::file %)]}
  (let [fileMinus (mod (- (file a) (file b)) 24)]
                                 (cond (> fileMinus 12) (- 24 fileMinus) :else fileMinus)))
(s/fdef file-dist
        :args (s/cat :a ::pos :b ::pos)
        :ret ::file)

(defn opposite-file [a b] {:pre [(s/valid? ::pos a) (s/valid? ::pos b)]} (= (file-dist a b) 12))
(s/fdef opposite-file
        :args (s/cat :a ::pos :b ::pos)
        :ret boolean?)

(defn same-or-opposite-file [a b] {:pre [(s/valid? ::pos a) (s/valid? ::pos b)]} (= (mod (file-dist a b) 12) 0))
(s/fdef same-or-opposite-file
        :args (s/cat :a ::pos :b ::pos)
        :ret boolean?)

;; (defn canIDiagonal [^Pos from ^Pos to] (cond
;;                                      (= from to) {:short false :long false}
;;                                      :else (letfn [(equalsToFile [^int what] (= (.file to) what))
;;                                                    (eqModToFile [^int what] (equalsToFile (mod what 24)))]
;;                                              (cond
;;                                                (equalsToFile ))
