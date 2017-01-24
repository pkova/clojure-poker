(ns clojure-poker.core
  (:require [clojure-poker.evaluate :as eval]))

(defn game []
  {:players []
   :pot 0
   :biggest-bet 0
   :last-raise 0
   :deck (shuffle eval/deck)
   :board []
   :big-blind nil
   :small-blind nil
   :turn :pre-flop
   :last-player nil})

(defn player [name chips]
  {:name name
   :hand []
   :chips chips
   :bet 0
   :sit-out true
   :folded false})

(defn add-player [game player]
  (update game :players #(conj % player)))

(defn remove-player [game player]
  (update game :players #(vec (remove (partial = player) %))))

(defn deal
  ([game]
   (deal game 2))
  ([game hand-size]
   (let [player-count (count (:players game))
         hands (map vec (partition hand-size (take (* hand-size player-count) (:deck game))))]
     (-> (update game :players (fn [xs] (map-indexed #(assoc %2 :hand (nth hands %1)) xs)))
         (assoc :deck (drop (* hand-size player-count) (:deck game)))))))

(defn turn? [game player]
  (= (:turn game) player))

(defn playing? [player]
  (not (or (:sit-out player) (:folded player))))

(defn can-raise? [game player total]
  (and
   (turn? game player)
   (playing? player)
   (>= (:chips player) (- total (:bet player)))
   (<= (+ (:last-raise game) (:biggest-bet game)) total)))

(defn can-call? [game player]
  (and
   (turn? game player)
   (playing? player)
   (>= (:chips player) (:biggest-bet game))))

(defn can-check? [game player]
  (and
   (turn? game player)
   (playing? player)
   (= (:bet player) (:biggest-bet game))))

(defn round-over? [game]
  (= (:turn game) (:last-player)))

(defn- bet [game player total]
  (let [i (.indexOf (:players game) player)
        amount (- total (:bet player))
        updated-player (-> (update player :chips - amount)
                           (assoc :bet total))
        updated-game (-> (update game :pot + amount)
                         (assoc-in [:players i] updated-player))]
    (if (< (:biggest-bet game) total)
      (-> (assoc updated-game :biggest-bet total)
          (assoc :last-raise amount))
      updated-game)))

(defn raise [game player total]
  (if (can-raise? game player total)
    (-> (bet game player total)
        step)
    false))

(defn call [game player]
  (if (can-call? game player)
    (-> (bet game player (:biggest-bet game))
        step)
    false))

(defn check [game player]
  (if (can-check? game player)
    (step game)
    false))


