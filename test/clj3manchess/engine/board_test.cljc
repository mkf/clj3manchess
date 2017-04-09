(ns clj3manchess.engine.board-test
  (:require [clj3manchess.engine.board :as b]
            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])))

(t/deftest newgame-map []
  (println (b/fill-map-board ::b/newgame))
  (println "\n")
  (println (b/getb ::b/newgame [0 0]))
  (println "\n")
  (println (b/string-of-arrayboard (b/fill-array-board ::b/newgame))))
