(ns sonyahon.rdk-clj.rng.core
  (:require [xoroshiro128.core :as x]))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn rng
  "Returns an atom of rng. Rest of the function will mutate the state of this atom"
  ([] (rng (System/currentTimeMillis)))
  ([seed] (atom (x/xoroshiro128+ seed))))

(defn next-state
  "Advance rng to its next state, without mutation of the rng"
  [rng]
  (x/next rng))

#_{:clj-kondo/ignore [:redefined-var]}
(defn rand
  "Get a float [0..1), and mutate the state of passed rng"
  ([rng] (let [val (x/long->unit-float (x/value @rng))]
           (reset! rng (next-state @rng))
           val)))

#_{:clj-kondo/ignore [:redefined-var]}
(defn rand-int
  "Same as `rand` but returns an int form start to end.
   If start is not provided 0 is used instread"
  ([rng end] (rand-int rng 0 end))
  ([rng start end]
   (let [val (rand rng)]
     (int (Math/floor (+ start (* val (- end start))))))))

(defn rand-item
  "Selects a random item of the collection, mutates the state of rng"
  [rng coll]
  (let [ind (rand-int rng (count coll))]
    (nth coll ind)))

#_{:clj-kondo/ignore [:redefined-var :clojure-lsp/unused-public-var]}
(defn shuffle
  "Shuffles elements of collection around. Mutates the state of passed rng several times"
  [rng coll]
  (loop [resp []
         c coll]
    (if (empty? c)
      resp
      (let [item (rand-item rng c)]
        (recur
         (conj resp item)
         (remove #(= % item) c))))))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn weighted
  "Accept an rng and a map with keys and their arbitrary weight
   e.g. {:x 1 :y 2 :z 5}
   Returns a random key of passed map, based on the weight values passed.
   Mutates the state of passed rng."
  [rng coll]
  (let [total (reduce + (vals coll))
        r (* (rand rng) total)]
    (loop [ks (keys coll)
           part 0]
      (let [npart (+ part (coll (first ks)))]
        (if (< r npart)
          (first ks)
          (recur
           (rest ks)
           npart))))))


