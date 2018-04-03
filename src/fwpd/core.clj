(ns fwpd.core
  (:gen-class))

(def filename "resources/suspects.csv")

(def vamp-keys [:name :glitter-index])

(defn str->int
  "Convert a string to an integer"
  [str]
  (Integer. str))

(def conversions
  {:name          identity
   :glitter-index str->int})

(defn convert
  "Convert a value based on the vamp-key"
  [vamp-key value]
  ((get conversions vamp-key) value))

(defn parse
  "Convert a CSV into rows of columns"
  [string]
  (map #(clojure.string/split % #",")
       (clojure.string/split string #"\n")))

(defn convert-row
  "Convert a row from suspects csv to a suspects map"
  [unmapped-row]
  (reduce
   (fn [row-map [vamp-key value]]
     (assoc row-map vamp-key (convert vamp-key value)))
   {}
   (map vector vamp-keys unmapped-row)))

(defn mapify
  "Return a seq of maps like {:name \"Edward Cullen\" :glitter-index 10}"
  [rows]
  (map convert-row rows))

(defn glitter-filter
  "Return all records that have a glitter-index of at least the input minimum glitter"
  [minimum-glitter records]
  (filter #(>= (:glitter-index %) minimum-glitter) records))

(def original_suspects (into [] (mapify (parse (slurp filename)))))
(def extra_suspects [{:name "Count Dracula" :glitter-index 10}
                     {:name "Princess Sparkles" :glitter-index 2}])

(defn vampires
  "Return a list of vampires based on a list of suspects"
  ([]
   (vampires original_suspects))
  ([suspects]
   (glitter-filter 3 suspects)))

(defn names-of
  "Return the names of the given suspects"
  [suspects]
  (clojure.string/join ", " (map #(:name %) suspects)))

(defn append
  "Append a new suspect to list of suspects"
  [current-suspects new-suspect]
  (conj current-suspects new-suspect))

(def all_suspects
  (reduce
   append
   original_suspects
   extra_suspects))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println (names-of (vampires all_suspects))))
