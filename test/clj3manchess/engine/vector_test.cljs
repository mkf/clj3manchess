(ns clj3manchess.engine.vector-test
  (:require  [clojure.test :as t]
             [clj3manchess.engine.vectors :as v]))

(with-test
  (is (= {:inward true} (first (v/units {:inward true :abs 3} 4)))))

