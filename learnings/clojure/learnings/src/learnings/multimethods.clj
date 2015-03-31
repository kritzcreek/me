(ns learnings.multimethods
  "Learnings regarding multimethods")

;; with help from: http://adambard.com/blog/structured-clojure-protocols-and-multimethods/

;; Let's say you have some abstract function that takes a list of prices
;; and calculates the average. This is great if all of the prices you care
;; about are in list data structures, but what happens if one of your price-
;; lists is in a map? Or a vector? You *could* make an `if' statement that
;; checks the type of the input, but that can get hairy. Instead, you shoul
;; use multimethods.
;;
;; Before using multimethods, you code might look like this:

(declare do-a-thing-with-a-vector)
(declare do-a-thing-with-a-map)

(defn do-a-thing
  "Do a thing to a vec or hash-map"
  [in]
  (cond
   (instance? clojure.lang.PersistentVector in)
   (do-a-thing-with-a-vector in)
   (instance? clojure.lang.PersistentArrayMap in)
   (do-a-thing-with-a-map in)))

(defn do-a-thing-with-a-vector [in]
  "A vector (via function)")

(defn do-a-thing-with-a-map [in]
  "A map (via function)")

;; In the above code, if I want to add an implementation for a list, I have to
;; change the code in three places. Here is the code written using a multimethod:

(defmulti do-a-thing class)

(defmethod do-a-thing clojure.lang.PersistentVector [in]
  "A vector (via multimethod)")

(defmethod do-a-thing clojure.lang.PersistentArrayMap [in]
  "A map (via multimethod)")

;; Now, to add a new operation, I just need to add a new `defmethod.' Brilliant!
