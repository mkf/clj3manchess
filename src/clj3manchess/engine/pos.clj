(ns clj3manchess.engine.pos
  (:use [clj3manchess.engine.color]
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

(defn posColorSegm [pos] {:pre [(s/valid? ::pos pos)]} (colors (quot (file pos) 8)))
(s/fdef posColorSegm
        :args (s/cat :pos ::pos)
        :ret ::color)

(defn posOnSegm [color rank file-on-segm] {:pre [(s/valid? ::color color) (s/valid? ::rank rank) (s/valid? ::file-on-segm file-on-segm)]}
  [rank (+ fileOnSegm
           (bit-shift-left (colorSegm color) 3))])
(s/fdef posOnSegm
        :args (s/cat :color ::color :rank ::rank :file-on-segm ::file-on-segm)
        :ret ::pos)

(defn sameRank [a b] {:pre [(s/valid? ::pos a) (s/valid? ::pos b)]} (= (rank a) (rank b)))
(s/fdef sameRank
        :args (s/cat :a ::pos :b ::pos)
        :ret boolean?)

(defn sameFile [a b] {:pre [(s/valid? ::pos a) (s/valid? ::pos b)]} (= (file a) (file b)))
(s/fdef sameFile
        :args (s/cat :a ::pos :b ::pos)
        :ret boolean?)

(defn fileDist [a b] {:pre [(s/valid? ::pos a) (s/valid? ::pos b)] :post [(s/valid? ::file %)]}
  (let [fileMinus (mod (- (file a) (file b)) 24)]
>>>>>>> dd12f1518f47f755ec220adbd0df074abb4d3d3b
                                 (cond (> fileMinus 12) (- 24 fileMinus) :else fileMinus)))
(s/fdef fileDist
        :args (s/cat :a ::pos :b ::pos)
        :ret ::file

(defn adjacentFile [a b] {:pre [(s/valid? ::pos a) (s/valid? ::pos b)]} (= (fileDist a b) 12))
(s/fdef adjacentFile
        :args (s/cat :a ::pos :b ::pos)
        :ret boolean?)

(defn sameOrAdjacentFile [a b] {:pre [(s/valid? ::pos a) (s/valid? ::pos b)]} (= (mod (fileDist a b) 12) 0))
(s/fdef sameOrAdjacentFile
        :args (s/cat :a ::pos :b ::pos)
        :ret boolean?)

;; (defn canIDiagonal [^Pos from ^Pos to] (cond
;;                                      (= from to) {:short false :long false}
;;                                      :else (letfn [(equalsToFile [^int what] (= (.file to) what))
;;                                                    (eqModToFile [^int what] (equalsToFile (mod what 24)))]
;;                                              (cond
;;                                                (equalsToFile ))
