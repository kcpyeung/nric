(def prefix {"S" 0, "T" 4})

(def weights [2 7 6 5 4 3 2])

(def check-digits {0 "J", 1 "Z", 2 "I", 3 "H", 4 "G", 5 "F", 6 "E", 7 "D", 8 "C", 9 "B", 10 "A"})

(defn nric [n]
  (let [atoms (clojure.string/split n #"")
        letter-value (prefix (first atoms))
        numbers (map #(Integer. %) (drop 1 atoms))]
    (as-> numbers v
         (map vector weights v)
         (map (fn [p] (* (first p) (second p))) v)
         (reduce + v)
         (+ v letter-value)
         (rem v 11)
         (check-digits v)
         (str n v))))

(defn get-training-nric [prefix how-many]
  (take how-many (map nric (map #(format (str prefix "%07d") %)(repeatedly #(rand-int 10000000))))))

(defn as-csv [prefix how-many]
  (let [data (->> (get-training-nric prefix how-many)
                  (map #(clojure.string/split % #""))
                  (map #(clojure.string/join "," %)))]
  (doseq [nric data]
    (println nric))))

(as-csv "S" 50)
