(ns clj3manchess.engine.vector-test
  (:require [cljs.test :refer-macros [with-test is]]
            [clj3manchess.engine.vectors :as v]))

(with-test
  (is (= {:inward true} (first (v/units {:inward true :abs 3} 4)))))

