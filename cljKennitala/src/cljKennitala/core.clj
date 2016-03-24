(ns cljKennitala.core
  (:gen-class))

(defn isValid [kennitala]
  (if (not= (count kennitala) 10)
    false
    (do
      (def numbers (map #(Character/digit % 10) kennitala))
      (def number
        (- 11 (mod (reduce + (map * numbers '(3, 2, 7, 6, 5, 4, 3, 2))) 11)))

      (if (= number 11)
        (def number 0)
        false)

      (= number (nth numbers 8)))))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
