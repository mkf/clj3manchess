(ns clj3manchess.engine.color
  (:require [schema.core :as s :include-macros true]
            #?(:clj [clojure.spec :as sc]
               :cljs [cljs.spec :as sc])
            [clojure.set :as set]))

(def Color (s/enum :white :gray :black))
(def colors [:white :gray :black])
(def colorset (set colors))
(sc/def ::color colorset)
(def colors-w-nil [nil :white :gray :black])
(def ColorSegm (s/enum 0 1 2))
(def ColorIdx (s/enum 1 2 3))

(s/defn segm :- ColorSegm [color :- Color]
  (case color :white 0 :gray 1 :black 2 nil))
(sc/fdef segm :args (sc/cat :color ::color) :ret #{0 1 2})

(s/defn idx :- ColorIdx [color :- Color] (when color (inc (segm color))))
(sc/fdef idx :args (sc/cat :color ::color) :ret #{1 2 3})

(s/defn next-col :- Color [color :- Color] (case color
                                               :white :gray
                                               :gray  :black
                                               :black :white))
(sc/fdef next-col :args (sc/cat :color ::color) :ret ::color)

(s/defn prev-col :- Color [color :- Color] (case color
                                               :white :black
                                               :gray  :white
                                               :black :gray))
(sc/fdef prev-col :args (sc/cat :color ::color) :ret ::color)

(def charset #{\w \g \b})
(def colchar {:white \w :gray \g :black \b})
(def charcol (set/map-invert colchar))
