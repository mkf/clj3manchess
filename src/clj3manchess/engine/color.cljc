(ns clj3manchess.engine.color
  (:require [schema.core :as s :include-macros true]))

(def Color (s/enum :white :gray :black))
(def colors [:white :gray :black])
(def ColorSegm (s/enum 0 1 2))
(def ColorIdx (s/enum 1 2 3))

(s/defn segm :- ColorSegm [color :- Color]
  (case color :white 0 :gray 1 :black 2 nil))

(s/defn idx :- ColorIdx [color :- Color] (when color (inc (segm color))))

(s/defn next-col :- Color [color :- Color] (case color
                                               :white :gray
                                               :gray  :black
                                               :black :white))

(s/defn prev-col :- Color [color :- Color] (case color
                                               :white :black
                                               :gray  :white
                                               :black :gray))
