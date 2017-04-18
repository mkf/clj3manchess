(ns clj3manchess.engine.pos
  (:require
    [clj3manchess.engine.color :as col]
    [clojure.spec :as s]))

;(defrecord Pos [rank file])
(s/def ::rank (set (range 6)))
(s/def ::file (set (range 24)))
(s/def ::file-on-segm (set (range 8)))
(s/def ::pos (s/tuple ::rank ::file))

(defn rank [[rank file]] rank)
(defn file [[rank file]] file)

(def kfm 4)

(s/defn color-segm :- col/Color [pos :- Pos] (col/colors (quot (file pos) 8)))

(s/defn pos-on-segm :- Pos [color :- col/Color, rank :- Rank, file-on-segm :- FileOnSegm]
  [rank (+ file-on-segm
           (bit-shift-left (col/segm color) 3))])

(s/defn same-rank :- s/Bool [a :- Pos, b :- Pos] (= (rank a) (rank b)))
(s/defn same-file :- s/Bool [a :- Pos, b :- Pos] (= (file a) (file b)))

(s/defn file-dist :- File [a :- Pos, b :- Pos]
  (let [fileminus (mod (- (file a) (file b)) 24)]
    (cond (> fileminus 12) (- 24 fileminus) :else fileminus)))

(s/defn opposite-file :- s/Bool [a :- Pos, b :- Pos] (= (file-dist a b) 12))

(s/defn same-or-opposite-file :- s/Bool [a :- Pos, b :- Pos] (zero? (mod (file-dist a b) 12)))

;; (defn canIDiagonal [^Pos from ^Pos to] (cond
;;                                      (= from to) {:short false :long false}
;;                                      :else (letfn [(equalsToFile [^int what] (= (.file to) what))
;;                                                    (eqModToFile [^int what] (equalsToFile (mod what 24)))]
;;                                              (cond
;;                                                (equalsToFile ))

(def all-pos
  (->> (range 6)
       (map (fn [rank] (->> (range 24)
                            (map (fn [file] [rank, file])))))
       (apply concat)))

