(ns clojure-poker.core
  (:require [clojure-poker.evaluate :as eval]))

(defn game []
  {:players []
   :pot 0
   :biggest-bet 0
   :deck (shuffle eval/deck)
   :board []
   :big-blind nil
   :small-blind nil
   :turn nil})

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

