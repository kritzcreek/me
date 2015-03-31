(ns learnings.async
  "Rock-paper-scissors with core.async
  http://tech.puredanger.com/2013/07/10/rps-core-async/"
  (:require [clojure.core.async :as async :refer :all]
            [org.httpkit.client :as http]
            [cheshire.core :as cheshire]
            [clojure.pprint :refer [pprint]]))

;; other helpful resources:
;; - https://www.youtube.com/watch?v=msv8Fvtd6YQ
;; - https://www.youtube.com/watch?v=enwIIGzhahw


;; Game definitions
(def moves [:rock :paper :scissors])
(def beats {:rock :scissors, :paper :rock, :scissors :paper})

;; This player randomly throws moves on an output channel:
(defn rand-player
  "Create a named player and return a channel to report moves."
  [name]
  (let [out (chan)]
    (go (while true (>! out [name (rand-nth moves)])))
    out))

;; `chan' creates an unbuffered (0-length) channel. We create a `go' process,
;; which you can think of as a lightweight thread, that will loop forever
;; creating random moves (represented as a vector of [name throw]) and placing
;; them on the out channel.
;;
;; However, because the channel is unbuffered, the put (`>!') will not succeed
;; until someone is ready to read it. Inside a go process, these puts will NOT
;; block a thread; the go process is simply parked waiting.
;;
;; To create our judge, we'll need a helper method to decide the winner:

(defn winner
  "Based on two moves, return the name of the winner."
  [[name1 move1] [name2 move2]]
  (cond
   (= move1 move2) "Nohbudy"
   (= move2 (beats move1)) name1
   :else name2))

;; And now we're ready to create our judging process:

(defn judge
  "Given two channels on which players report moves, create and return an
  output channel to report the results of each match as [move1 move2 winner]."
  [p1 p2]
  (let [out (chan)]
    (go
      (while true
        (let [m1 (<! p1)
              m2 (<! p2)]
          (>! out [m1 m2 (winner m1 m2)]))))
    out))

;; The judge is a go process that sits in a loop forever. Each time through the
;; loop it takes a move from each player, computes the winner, and reports the
;; match results on the out channel (as [move1 move2 winner]).
;;
;; We need a bit of wiring code to start up the players and the judge and get
;; the match result channel:

(defn init
  "Create 2 players, by default Alice and Bob, and return an output channel of
  match results."
  ([] (init "Alice" "Bob"))
  ([n1 n2] (judge (rand-player n1) (rand-player n2))))

;; And then we can play the game by simply taking a match result from the output
;; channel and reporting.

(defn report
  "Report results of a match to the console."
  [[name1 move1] [name2 move2] winner]
  (println)
  (println name1 " throws " move1)
  (println name2 " throws " move2)
  (println winner " wins!"))

(defn play
  "Play by taking a match reporting the results of the latest match."
  [out-chan]
  (apply report (<!! out-chan)))

;; Here we use the actual blocking form of take (<!!) since we are outside a go
;; block in the main thread.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @halgari's example from Clojure Conj 2013

(defn http-get [url]
  (let [c (chan)]
    (println url)
    (http/get url
              (fn [r] (put! c r)))
    c))


(def key "b4cb6cd7a349b47ccfbb80e05a601a7c")


(defn request-and-process [url]
  (go
    (-> (str "http://api.themoviedb.org/3/" url "api_key=" key)
        http-get
        <!
        :body
        (cheshire/parse-string true))))

(defn latest-movies []
  (request-and-process "movies/latest?"))

(defn top-rated-movies []
  (request-and-process "movies/top_rated?"))

(defn movie-by-id [id]
  (request-and-process (str "movie/" id "?")))

;(go (println (<! (movie-by-id 238))))

(defn movie-cast [id]
  (request-and-process (str "movie/" id "/casts?")))

;(go (println (<! (movie-cast 238))))

(defn people-by-id [id]
  (request-and-process (str "person/" id "?")))

;(go (println (<! (people-by-id 3144))))

(defn avg [col]
  (-> (clojure.core/reduce + 0 col)
      (/ (count col))))
