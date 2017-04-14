(ns clj3manchess.engine.board-test
  (:require [clj3manchess.engine.board :as b]
            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])))

(t/deftest newgame-map []
  (println (b/string-of-arrayboard (b/fill-array-board (b/fill-map-board ::b/newgame) ) ))
  (println "\n")
  (println (b/getb ::b/newgame [0 0]))
  (println "\n")
  (println (b/string-of-arrayboard (b/fill-array-board ::b/newgame))))

(t/deftest newgame-where-are-bishops []
  (t/is (b/where-are-figs-of-type ::b/newgame :bishop) '([0 2] [0 5] [0 10] [0 13] [0 18] [0 21])))
