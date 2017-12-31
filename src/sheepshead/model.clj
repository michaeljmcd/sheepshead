(ns sheepshead.model)

(defn create-deck []
  (let [suits [:hearts :clubs :spades :diamonds]
        ranks (range 7 15)]
    (flatten (mapv (fn [a] (mapv #(do {:rank a :suit %}) suits)) ranks))))

(defn shuffle-deck [deck]
  (shuffle deck))

(defn is-jack? [card] (= (:rank card) 11))

(defn is-queen? [card] (= (:rank card) 12))

(defn is-trump? [card]
  (or (= (:suit card) :diamonds)
      (is-jack? card)
      (is-queen? card)))
