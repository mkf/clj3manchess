(ns clj3manchess.engine.color
  (:require [clojure.spec :as s]))

(s/def ::color #{:white :gray :black})
(def colors [:white :gray :black])

(defn segm [color] (case
                             :white 0
                             :gray 1
                             :black 2))
(s/fdef segm
        :args (s/cat :color ::color)
        :ret #{0 1 2})

(defn idx [color] (inc (segm color)))
(s/fdef idx
        :args (s/cat :color ::color)
        :ret #{1 2 3})

(defn next [color] (case
                            :white :gray
                            :gray :black
                            :black :white))
(s/fdef next
        :args (s/cat :color ::color)
        :ret ::color)

(defn prev [color] (case
                            :white :black
                            :gray :white
                            :black :gray))
(s/fdef prev
        :args (s/cat :color ::color)
        :ret ::color)


