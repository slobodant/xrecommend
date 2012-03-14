(ns xrecommend.matrixFactorization
  (:gen-class)
  (:use (incanter core stats charts io datasets))
  (:use xrecommend.parser))

; recursion example
(defn countdown [result x] 
  (if (zero? x)
    result
    (recur (conj result x) (dec x))))

;random matrix calculation
(defn random-matrix [rows columns]
  (matrix (apply vector (take (* rows columns) (repeatedly rand))) rows))

(defn calculate-Eij [R P Q i j]
  (if (> (sel R :rows i :cols j) 0)
  (- (sel R :rows i :cols j) (mmult (sel P :rows i) (sel Q :cols j)))
  0))


(defn init-values [alpha beta1 K rows columns R] 
  (assoc {} :alpha alpha :beta beta1 :K K :P (random-matrix K rows) :Q (random-matrix columns K) :E (matrix 0 rows columns) :R R))

(defn get-Eij [R P Q] 
  (matrix (for [i (range 0 (count R))] 
  (for [j (range 0 (count (trans R)))] 
  (calculate-Eij R P Q i j)))))

(defn calculate-P [mapa Eij] 
  (plus (:P mapa) (mult (:alpha mapa) (minus (mult 2 (mmult Eij (trans (:Q mapa)))) (mult (:beta mapa) (:P mapa))))))

(defn calculate-Q [mapa Eij]
  (plus (:Q mapa) (mult (:alpha mapa) (minus (mult 2 (trans (mmult (trans Eij) (:P mapa)))) (mult (:beta mapa) (:Q mapa))))))

;calculating small "e"
(defn count-eB [P Q beta1 K] 
  (reduce (fn [res row] 
          (* (/ beta1 2) (+ (pow (sum (sel (trans P) :rows row)) 2) (pow (sum (sel Q :rows row)) 2)))) (range 0 K)))

;update result, method for calculating update for one step
(defn update-result [mapa]
  (let [P (:P mapa)
        Q (:Q mapa)
        E (:E mapa)
        alpha (:alpha mapa)
        beta (:beta mapa)
        R (:R mapa)
        K (:K mapa)
        Eij (get-Eij R P Q)
        eB (count-eB P Q beta K)]
    (assoc mapa :P (calculate-P mapa Eij) 
                :Q (calculate-Q mapa Eij) 
                :E (plus E (pow (minus R (mmult P Q))) (matrix eB (count (sel R :cols 0)) (count (sel R :rows 0)))))))

;final method calculate-all
;calculates final value for R~ matrix
(def all-movies (get-all-movies critics))
(def all-users (get-users critics))
(def R (create-R-matrix critics all-movies all-users))
(def mapa (init-values 0.0002 0.02 2 (count (sel R :cols 0)) (count (sel R :rows 0) ) R))

(defn calculate-all [mapa step]
  (if (zero? step)
    mapa
    (recur (update-result mapa) (dec step))))


(def result (calculate-all mapa 5000))
(def R-prim-matrix (mmult (:P result) (:Q result)))

(defn get-movies-recommendation [user all-movies all-users matrix] 
   (sort-by #(val %) > (reduce (fn [result movie] (assoc result movie (nth (sel matrix :rows (.indexOf all-users user)) (.indexOf all-movies movie)))) {} all-movies)))

(defn get-recommendations [user critics-map all-movies all-users matrix]
  (reduce (fn [mapa entry] (dissoc mapa (key entry))) (get-recommended-movies user all-movies all-users R-prim-matrix) (critics-map user)))
  
 

