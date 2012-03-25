(ns xrecommend.parser
    (:use (incanter core stats charts io datasets)))

(use '[clojure.contrib.duck-streams :only (reader)])
(use '[clojure.contrib.string :only (split)])
(def critics {"Lisa Rose" {"Lady in the Water" 2.5, "Snakes on a Plane" 3.5,
                           "Just My Luck" 3.0, "Superman Returns" 3.5, "You, Me and Dupree" 2.5,
                           "The Night Listener" 3.0}, "Gene Seymour" {"Lady in the Water" 3.0, "Snakes on a Plane" 3.5,
                                                                      "Just My Luck" 1.5, "Superman Returns" 5.0, "The Night Listener" 3.0,
                                                                      "You, Me and Dupree" 3.5}, "Michael Phillips" {"Lady in the Water" 2.5, "Snakes on a Plane" 3.0,
                                                                                                                     "Superman Returns" 3.5, "The Night Listener" 4.0}, "Claudia Puig" {"Snakes on a Plane" 3.5, "Just My Luck" 3.0,
                                                                                                                                                                                        "The Night Listener" 4.5, "Superman Returns" 4.0,
                                                                                                                                                                                        "You, Me and Dupree" 2.5}, "Mick LaSalle" {"Lady in the Water" 3.0, "Snakes on a Plane" 4.0,
                                                                                                                                                                                                                                   "Just My Luck" 2.0, "Superman Returns" 3.0, "The Night Listener" 3.0,
                                                                                                                                                                                                                                   "You, Me and Dupree" 2.0}, "Jack Matthews" {"Lady in the Water" 3.0, "Snakes on a Plane" 4.0,
                                                                                                                                                                                                                                                                               "The Night Listener" 3.0, "Superman Returns" 5.0, "You, Me and Dupree" 3.5}, "Toby" {"Snakes on a Plane" 4.5,"You, Me and Dupree" 1.0,"Superman Returns" 4.0}})
;matrix factorization part
(defn parse-critics [critics-map]
  (map val critics-map))

(defn helper-function [movies-map]
  (map key movies-map))

(defn get-users [critics-map]
  (map key critics-map))

(defn get-movies-for-user [user critics-map]
  (map key (critics-map user)))
  
(defn seq-contains?
  "Determine whether a sequence contains a given item"
  [sequence item]
  (if (empty? sequence)
    false
    (reduce #(or %1 %2) (map #(= %1 item) sequence))))

(defn update-list [list1 list2]
  (reduce (fn [list element] (if (seq-contains? list element) list (conj list element))) list1 list2))

(defn get-all-movies [critics-map]
(reduce (fn [result entry] (update-list result (get-movies-for-user entry critics-map))) () (get-users critics-map)))

(defn row-user-critics [critics-user all-movies]
  (reduce (fn [lista movie] 
          (if (contains? critics-user movie) 
            (conj lista (critics-user movie)) 
            (conj lista 0))) [] all-movies))


(defn create-R-matrix [critics-map all-movies all-users]
  (matrix (reduce (fn [result user] (conj result (row-user-critics (critics-map user) all-movies ))) [] all-users)))


; reading dataset part

;functions for getting movie data from file
(defn assoc-key-val [res stringa]
  (let [splitted (split #"\|" stringa)]
    (assoc res (first splitted) (second splitted))))

(defn get-key-vals [string-list]
  (reduce assoc-key-val {} string-list))

(def movies (line-seq (reader "data/test.item")))
(def data (line-seq (reader "data/test.data")))

movies

;fill movies map
(def movies-map (get-key-vals movies))

;fill final map for calculating
(defn assoc-sec-key-value-data [res stringa]
  (let [splitted (split #"\t" stringa)]
    (assoc res (movies-map (nth splitted 1)) (Double/parseDouble (nth splitted 2)))))

(defn assoc-key-val-data [res stringa]
  (let [splitted (split #"\t" stringa)]
    (assoc res (first splitted) (assoc-sec-key-value-data {} stringa))))

;update map with all values

(defn assoc-key-val-data-2 [res stringa]
  (let [splitted (split #"\t" stringa)]
    (if (res (nth splitted 0))
      (assoc res (nth splitted 0) (assoc-sec-key-value-data (res (nth splitted 0)) stringa))
      (assoc res (nth splitted 0) (assoc-sec-key-value-data {} stringa)))))



(defn get-key-vals-data [string-list]
  (reduce assoc-key-val-data {} string-list))


;Get all critics by USER
(defn get-key-vals-data-2 [string-list]
  (reduce assoc-key-val-data-2 {} string-list))

(def critics-map (get-key-vals-data-2 data))


