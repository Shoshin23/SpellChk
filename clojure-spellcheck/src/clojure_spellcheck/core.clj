(ns spellcheck.core
  (:use clojure.set))

(def alphabet "abcdefghijklmnopqrstuvwxyz")

(defn- words [text] (re-seq #"[a-z]+" (.toLowerCase text)))

(defn- train [features]
  (reduce (fn [model f] 
            (assoc model f (inc (get model f 1)))) {} features))

(def NWORDS (train (words (slurp "big.txt"))))

(defn- deletes [word]
  "Returns all deletions (an arbitrary letter is removed) for the word"
  (set (for [i (range (count word))] 
                (str (subs word 0 i) (subs word (inc i))))))

(defn- replaces [word]
  "Returns all replacements (where a letter is replaced with a letter 
   in the alphabet) for a word"
  (for [i (range (count word)) c alphabet]
    (str (subs word 0 i) c (subs word (inc i)))))

(defn- inserts [word]
  "Returns all insertions (where a letter is inserted at an arbitrary 
   location) within the word"
  (for [i (range (inc (count word))) c alphabet]
    (str (subs word 0 i) c (subs word i))))

(defn- transposes [word]
  "Returns all transpositions (where adjacent letters are swapped) 
   of a string"
  (for [i (range (count word)) :when (> (count (subs word i)) 1)]
    (str (subs word 0 i) (nth word (inc i)) 
         (nth word i) (subs word (inc (inc i))))))

(defn edits1 [word]
  "Returns a seq of all strings a distance of 1 from word"
  (let [w word]
    (distinct (concat (deletes w) (replaces w) 
                      (inserts w) (transposes w)))))

(defn known [edits]
  "Returns a seq of all words known (as defined by NWORDS)"
  (seq (filter #(NWORDS %) edits)))

; Memoization should dramatically increase the speed...
(def edits1 (memoize edits1))
(def known (memoize known))

(defn correct [word]
  "Here we estimate P(w|c): a known word of edit length 0 is infinitely
  more likely than a known word of edit length 1, which is infinitely more likely
  than a known word of edit length 2."
  (let [candidates (or (known [word]) 
                       (known (edits1 word)) 
                       (known (map edits1 (known (edits1 word))))
                       [word])]
    (apply max-key #(NWORDS %) candidates)))
