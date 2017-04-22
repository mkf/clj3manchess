(ns clj3manchess.engine.vector-test
  (:use [#?(:clj clojure.test :cljs cljs.test) :only [run-tests is deftest]])
  (:require [clj3manchess.engine.vectors :as v]))

(deftest simple-adding-rankvector []
  (is (= (v/addvec {:inward true} [2,2]) [3,2]))
  (is (= (v/addvec {:inward true :abs 3} [2,2]) [5,2])))

(deftest first-inward3from4-units []
  (is (= {:inward true} (first (v/units-rank-vec {:inward true :abs 3} 4))))
  (is (v/is-rankvec? {:inward true}))
  (is (= {:inward true} (first (v/units {:inward true :abs 3} 4)))))

(run-tests)
