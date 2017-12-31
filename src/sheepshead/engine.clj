(ns sheepshead.engine
  (:require [sheepshead.model :as m]))

; A few things probably bear consideration here. First, we must draw a
; distinction between a game and a hand. A game consists of multiple hands and
; the objective of each hand is to score over 61 points.
; 
; We will take a cue from Sheepshead tournament rules and assume that a full
; game is 3 full passes around the "table".

; As for a gamespec, we need a format. Let's start with this:
; { :players [{:display-name STRING }] :end-condition { :rounds 5 } }

(defn create-hand [game]
  { 
   :state :deal 
   :players (:players game)
  })

(def blind-counts
  #^{:private true}
  { 3 2 4 4 5 2 })

(defn- distribute-packets [players packets result]
  (if (empty? players)
    (reverse result)
    (recur (rest players)
           (rest packets)
           (cons (assoc (first players)
                        :hand
                        (first packets))
                 result))

  ))

; TODO: fix this and make it more classically "fair".
(defn- distribute-cards [hand cards]
  (let [player-count (-> hand :players count)
        blind-count (get blind-counts player-count)
        hand-size (/ (- (count cards) blind-count) player-count)
        packets (partition hand-size hand-size nil cards)]
    ; packets contains collections for each players hands, plus a tail
    ; collection with the blind.
    (-> hand
        (assoc :blind (last packets))
        (assoc :players (distribute-packets (:players hand) (drop-last packets) nil))
        )
  ))

(defn- deal-hand [hand input]
  (let [cards (m/shuffle-deck (m/create-deck))]
    hand
    (-> hand
      (assoc :state :auction)
      (distribute-cards cards)
    )
  ))

(def hand-state-functions
  #^{:private true}
  {
    :deal deal-hand
  })

(defn advance-hand [hand input]
  (apply (-> :state hand hand-state-functions) 
         [hand input]))

(def requirement-state-functions
  #^{:private true}
  {
   :deal (fn [hand] nil)
  })

; This function is used to query what input is required to advance a hand.
(defn input-requirement [hand]
  (apply (-> :state hand requirement-state-functions)
         [hand]))
