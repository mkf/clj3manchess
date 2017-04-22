(ns clj3manchess.engine.move-test
  (:require [clj3manchess.engine.move :as sut :refer [eval-death]]
            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])
            [clj3manchess.engine.state :as st]
            [clj3manchess.engine.board :as b]))

(t/deftest eval-death-rem-king-gray []
  (t/is (= (:alive (eval-death (assoc st/newgame :board [::b/newgame {[0 12] nil}])))
           #{:white :black})))

(t/deftest amft-test []
  (t/is (= (sut/testing-tostring-amft [0 0] 4)
           "_X______X______X____X___
X_______X_______X___X___
________X________X__X__X
________X_________XXXXX_
________X_________XXXXX_
XXXXXXXXXXXXXXXXXXXX_XXX")))

(t/deftest knight-capturing-thru-moat-newgame []
  (t/is (= (sut/after-of-afters {:before st/newgame :from [0 1] :to [1 23]})
           :capturing-thru-moats)))
