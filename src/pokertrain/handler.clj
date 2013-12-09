(ns pokertrain.handler
  (:use compojure.core)
  (:require [compojure.handler :as handler]
            [compojure.route :as route]
            [hiccup.core :as h]))

(def scoring {:nothing 0
              :pair 1
              :two-pair 2
              :three-of-a-kind 3
              :straight 4
              :flush 5
              :full-house 6
              :four-of-a-kind 7
              :straight-flush 8})

(defn create-game [players]
  (->> (for [suit [:clubs :diamonds :hearts :spade]
             rank [:ace :two :three :four :five
                   :six :seven :eight :nine :ten
                   :jack :queen :king]]
         {:suit suit :rank rank})
       shuffle
       (take (* players 5))
       (partition 5)))
 
(defn group-hand [hand]
  (->> hand
       (map :rank)
       frequencies
       (filter #(<= 2 (% 1)))))

(defn pair-count [hand]
  (->> hand
       group-hand
       (filter #(= 2 (second %)))
       count))

(defn pair? [hand]
  (= 1 (pair-count hand)))

(defn two-pair? [hand]
  (= 2 (pair-count hand)))

(defn three-of-a-kind? [hand]
  (= 1
     (->> hand
          group-hand
          (filter #(= 3 (second %)))
          count)))

(defn full-house? [hand]
  (and (pair? hand)
       (three-of-a-kind? hand)))

(defn score-hand [hand]
  (cond (full-house? hand) (:full-house scoring)
        (three-of-a-kind? hand) (:three-of-a-kind scoring)
        (two-pair? hand) (:two-pair scoring)
        (pair? hand) (:pair scoring)
        :else (:nothing scoring)))

(defn score-game [game]
  (map (fn [player hand] {:player player :hand hand :score (score-hand hand)}) (range) game))

(defn winner [game]
  (let [game-score (score-game game)]
    (reduce (fn [acc player]
              (cond (> (:score player) (:score (first acc)))
                    #{player}
                    (= (:score player) (:score (first acc)))
                    (conj acc player)
                    :else acc))
            #{{:player :all :score 0}} game-score)))

(defroutes app-routes
  (GET "/" [] (h/html [:body
                       [:h1 (winner (create-game 4))]]))
  (route/resources "/")
  (route/not-found "Not Found"))

(def app
  (handler/site app-routes))
