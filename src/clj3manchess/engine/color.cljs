(ns clj3manchess.engine.color
  (:require [clojure.spec :as s]))

(s/def ::color #{:white :gray :black})
(def colors [:white :gray :black])

(defn colorSegm [color] (case
                             :white 0
                             :gray 1
                             :black 2))

(defn colorIndex [color] (+ 1 (colorSegm color)))

(defn nextColor [color] (case
                            :white :gray
                            :gray :black
                            :black :white))

(defn prevColor [color] (case
                            :white :black
                            :gray :white
                            :black :gray))



