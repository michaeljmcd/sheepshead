(ns sheepshead.engine-test
  (:require [clojure.test :refer :all]
            [sheepshead.engine :refer :all]))

(deftest basic-game-creation 
  (testing "Whatever."
    (let [game-spec {:players [{:display-name "foo"}
                               {:display-name "baz"}
                               {:display-name "baar"}]
                     :end-condition {:rounds 5}}]
      (create-game-fn game-spec))
    ))
