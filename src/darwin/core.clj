(ns darwin.core
  
  )

(defn swap-vector [v i1 i2]
  (assoc v i2 (v i1) i1 (v i2)))


(defn align-at-score [v1 v2 n]
  (if (= (v1 n) (v2 n)) 1 0))

(defn same-to-num [x y]
  (if (= x y) 1 0))

(defn order-fitness [collection]
  (let [sorted (sort collection)]
    (reduce + (map same-to-num collection sorted))))




(defn mutate [collection]
  (let [size (count collection)
        p0 (rand-int size)
        p1 (rand-int size)
        new (swap-vector collection p0 p1)
        ]
    new
    )
  )


(defn mutate-individual [individual]
  (conj {:chromosome ((:mutate-function individual) (:chromosome individual))}
        {:fitness (:fitness individual)}
        {:fitness-function (:fitness-function individual)}
        {:mutate-function (:mutate-function individual)}
        {:generations (inc (:generations individual))}
        )
  )

(defn copy-individual [individual]
  (conj {:chromosome (:chromosome individual)}
        {:fitness (:fitness individual)}
        {:fitness-function (:fitness-function individual)}
        {:mutate-function (:mutate-function individual)}
        {:generations (inc (:generations individual))}
        )
  )

(defn score-individual [individual]
  (conj {:chromosome (:chromosome individual)}
        {:fitness ((:fitness-function individual) (:chromosome individual))}
        {:fitness-function (:fitness-function individual)}
        {:mutate-function (:mutate-function individual)}
        {:generations (:generations individual)}
        )
  )



(defn prep-individual [fitness-function mutate-function individual]
  (conj {:chromosome (:chromosome individual)}
        {:fitness (fitness-function (:chromosome individual))}
        {:mutate-function mutate-function}
        {:fitness-function fitness-function}
        {:generations 0})
  )


(defn sort-population [population]
  (reverse (sort-by :fitness population))
  )

(defn print-top-fitness [sorted-population]
  (println (:fitness (first sorted-population)))
  )

(defn do-cycle [n population]
  (let [new-population (map mutate-individual population)
        new-scored-population (pmap score-individual new-population)
        copy-original (map copy-individual population)
        all (concat copy-original new-scored-population)
        top (take n (sort-population all))
        ]
    top
    )
  )

(defn prep-population [fitness-function mutate-function population]
  (sort-population (map (partial prep-individual fitness-function mutate-function) population)
                   ))

(defn evolve [stopping-function generations top population]
  (loop
    [p population
     g generations]
    (if (< g 0)
      p
      (if (stopping-function p)
        p
        (recur (do-cycle top p) (dec g))))
    )
  )

(defn generate-chromosome [n]
  (loop
    [v []
     i n]
    (if (< i 0)
      v
      (recur (conj v i) (dec i))))
  )




(defn my-fitness-function [chromosome]
  (order-fitness chromosome))

(defn my-mutate-function [chromosome]
  (mutate chromosome)
  )

(defn test-me [size generations top]

  (let [i1 {:chromosome (generate-chromosome size)}
        population (prep-population my-fitness-function my-mutate-function [i1])]

    (time
      (do
        (def b (evolve (fn [p] (> (:fitness (first p)) size)) generations top population))
        (println (:generations (first b)) (:fitness (first b)))
        )
      )
    ))

(test-me 256 1000000 8)




;(do-cycle population mutate-individual score-individual)