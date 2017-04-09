(ns clj3manchess.engine.fig
  (:require [schema.core :as s]
            [clj3manchess.engine.color :as c :refer [Color]]
            [clojure.string :as stri]))

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
           (s/required-key :crossedCenter) s/Bool})
(def Fig (s/either FigNotPawn Pawn))

(def figrunes {:pawn   {:white "P" :gray "Þ" :black "p"}
               :rook   {:white "R" :gray "®" :black "r"}
               :knight {:white "N" :gray "ń" :black "n"}
               :bishop {:white "B" :gray "¡" :black "b"}
               :queen  {:white "Q" :gray "Ω" :black "q"}
               :king   {:white "K" :gray "¥" :black "k"}})

(defn center-crossed-str [x] (cond (nil? x) " " (true? x) "¡" (false? x) "_"))

(s/defn crossed-center-str [fig :- Fig] (center-crossed-str (:crossedCenter fig)))

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
