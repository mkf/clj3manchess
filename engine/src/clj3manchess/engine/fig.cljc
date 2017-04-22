(ns clj3manchess.engine.fig
  (:require [schema.core :as s]
            [clj3manchess.engine.color :as c :refer [Color]]
            [clojure.string :as stri]
            [clojure.set :as set]))

(def figtypeset #{:pawn :rook :knight :bishop :queen :king})
(def figtypeset-sanspawn (remove #{:pawn} figtypeset))
(def FigType (apply s/enum figtypeset))
(def FigTypeNotPawn (apply s/enum figtypeset-sanspawn))

(def Piece {(s/required-key :type)  FigType
            (s/required-key :color) Color})

(def FigNotPawn {(s/required-key :type)  FigTypeNotPawn
                 (s/required-key :color) Color})
(def Pawn {(s/required-key :type)          (s/enum :pawn)
           (s/required-key :color)         Color
           (s/required-key :crossed-center) s/Bool})
(def Fig (s/either FigNotPawn Pawn))
(def promfigtypes #{:rook :knight :bishop :queen})
(def PromFigType (apply s/enum promfigtypes))

(def figrunes {:pawn   {:white "P" :gray "^" :black "p"}
               :rook   {:white "R" :gray "#" :black "r"}
               :knight {:white "N" :gray "ń" :black "n"}
               :bishop {:white "B" :gray "|" :black "b"}
               :queen  {:white "Q" :gray "Ω" :black "q"}
               :king   {:white "K" :gray "¥" :black "k"}})

(defn center-crossed-str [x] (cond (nil? x) " " (true? x) "¡" (false? x) "_"))

(s/defn crossed-center-str [fig :- Fig] (center-crossed-str (:crossed-center fig)))

(s/defn color-of-fig :- Color [fig :- Fig] (:color fig))
(s/defn type-of-fig :- FigType [fig :- Fig] (:type fig))
(defn getfigrunes [what fig] (if (nil? what) (println what fig) (what figrunes)))
(s/defn fig-by-type-rune [fig :- Fig] (getfigrunes (type-of-fig fig) fig))

(defn figcoltypestr [fig] ((color-of-fig fig) (fig-by-type-rune fig)))

(s/defn figstr :- s/Str
  [fig :- (s/maybe Fig)]
  (if (nil? fig) "__"
      (str (figcoltypestr fig)
           (crossed-center-str fig))))

(def figtypevec [nil :rook :knight :bishop :queen :king :pawn])
(def figtypebyidx (into {} (map-indexed vector figtypevec)))
(def figtypechars {:pawn \p :rook \r :knight \n :bishop \b :queen \q :king \k})
(def figtypebychar (set/map-invert figtypechars))
(def figtypeidces (set/map-invert figtypebyidx))
(s/defn figtoint :- s/Int [fig :- (s/maybe Fig)]
  (if (nil? fig) 0
      (bit-or (figtypeidces (:type fig))
              (bit-shift-left (c/segm (:color fig)) 3)
              (if (and (= (:type fig) :pawn)
                       (true? (get fig :crossed-center)))
                (bit-shift-left 1 6) 0))))
(s/defn inttofig :- (s/maybe Fig) [src :- (s/maybe s/Int)]
  (when (pos? src)
    (let [figtype (figtypebyidx (bit-and src 7))
          color (bit-and (bit-shift-right src 3) 7)
          color (when (and (<= 0 color) (< color 3)) (nth c/colors color))
          pc (when (= figtype :pawn) (bit-test src 6))]
      (cond-> {:color color :type figtype}
        (not (nil? pc)) (assoc :crossed-center pc)))))
