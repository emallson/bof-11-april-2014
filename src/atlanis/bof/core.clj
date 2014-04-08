(ns atlanis.bof.core)

(defn sieve
  "A functional implementation of the Sieve of Eratosthenes.

  Takes a list of numbers to perform the sieve on as its only parameter."
  [numbers]
  (loop [numbers numbers
         current-index 0]
    (if (< current-index (count numbers))
      (let [current-divisor (nth numbers current-index)]
        (recur (filter #(or (= % current-divisor)
                           (pos? (mod % current-divisor)))
                       numbers)
               (inc current-index)))
      numbers)))

(defn quicksort-split
  "Splitting function for quicksort. Returns a map containing :lesser, :equal
  and :greater values.

  This function is declared externally from quicksort because I don't want it re-declared on every iteration"
  [numbers pivot-element]
  (let [assoc-conj (fn [coll key val]
                     (assoc-in coll [key] (conj (key coll) val)))]
    (reduce (fn [collections number]
              (cond
               (< number pivot-element) (assoc-conj collections :lesser number)
               (= number pivot-element) (assoc-conj collections :equal number)
               :else (assoc-conj collections :greater number)))
            {} numbers)))

(defn quicksort
  "A functional recursive implementation of the Sieve of Eratosthenes.

  Takes a list of numbers to sort as its only parameter."
  [numbers]
  (if (<= (count numbers) 1)
    numbers
    (let [pivot-element (nth numbers (rand-int (count numbers)))
          split-result (quicksort-split numbers pivot-element)]
      (concat (quicksort (:lesser split-result))
              (:equal split-result)
              (quicksort (:greater split-result))))))