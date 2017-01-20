(ns clojure-poker.core
  (:require [clojure-poker.evaluate :as eval]))

(defn game []
  {:players []
   :pot 0
   :biggest-bet 0
   ;; :deck (shuffle eval/deck)
   :board []
   :big-blind nil
   :small-blind nil
   :turn nil})

(defn player [name chips]
  {:name name
   :hand []
   :chips chips
   :bet 0})

(defn add-player [game player]
  (update game :players #(conj % player)))

(defn remove-player [game player]
  (update game :players #(vec (remove (partial = player) %))))
