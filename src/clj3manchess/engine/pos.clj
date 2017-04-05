(ns clj3manchess.engine.pos
  (:use [clj3manchess.engine.color]))

(defrecord Pos [rank file])
(defn rank [^Pos pos] (.rank pos))
(defn file [^Pos pos] (.file pos))
(defn posColorSegm [^Pos pos] (get colors (quot (.file pos) 8)))
(defn posOnSegm [color rank fileOnSegm]
  (Pos. rank (+
              fileOnSegm
              (bit-shift-left (colorSegm color) 3))))
(defn sameRank [^Pos a ^Pos b] (= (.rank a) (.rank b)))
(defn sameFile [^Pos a ^Pos b] (= (.file a) (.file b)))
(defn fileDist [^Pos a ^Pos b] (let [fileMinus (mod (- (.file a) (.file b)) 24)]
                                 (cond (> fileMinus 12) (- 24 fileMinus) :else fileMinus)))
(defn adjacentFile [^Pos a ^Pos b] (= (fileDist a b) 12))
(defn sameOrAdjacentFile [^Pos a ^Pos b] (= (mod (fileDist a b) 12) 0))
;; (defn canIDiagonal [^Pos from ^Pos to] (cond
;;                                      (= from to) {:short false :long false}
;;                                      :else (letfn [(equalsToFile [^int what] (= (.file to) what))
;;                                                    (eqModToFile [^int what] (equalsToFile (mod what 24)))]
;;                                              (cond
;;                                                (equalsToFile ))
