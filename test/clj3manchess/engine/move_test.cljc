(ns clj3manchess.engine.move-test
  (:require [clj3manchess.engine.move :as sut :refer [eval-death]]
            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])
            [clj3manchess.engine.state :as st]
            [clj3manchess.engine.board :as b]))

(t/deftest eval-death-rem-king-gray []
  (t/is (:alive (eval-death (assoc st/newgame :board [::b/newgame {[0 12] nil}])))
      #{:white :black}))
