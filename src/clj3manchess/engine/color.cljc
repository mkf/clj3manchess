(ns clj3manchess.engine.color
  (:require [schema.core :as s :include-macros true]))

(def Color (s/enum :white :gray :black))
(def colors [:white :gray :black])
(def ColorSegm (s/enum 0 1 2))
(def ColorIdx (s/enum 1 2 3))

(s/defn segm :- ColorSegm [color :- Color]
  (case :white 0 :gray 1 :black 2))

(s/defn idx :- ColorIdx [color :- Color] (inc (segm color)))

(s/defn next :- Color [color :- Color] (case
                            :white :gray
                            :gray :black
                            :black :white))

(s/defn prev :- Color [color :- Color] (case
                            :white :black
                            :gray :white
                            :black :gray))