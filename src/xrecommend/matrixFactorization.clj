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
  (assoc {} :alpha alpha :beta beta1 :K K :P (random-matrix K rows) :Q (random-matrix columns K) :E [] :R R))

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

(defn count-e-beta1 [P Q beta K]
  (for [k (range 0 K)]
    (mult beta (mmult (sel P :rows k) (sel Q :cols k)))))

;calculate difference E
(defn calculate-error1 [R P Q beta alpha K]
  (sum (map sum 
         (for [i (range 0 (count R))] 
           (for [j (range 0 (count (trans R)))]
             (if (> (sel R :rows i :cols j) 0)
               ;calculation e = e + pow(R[i][j] - numpy.dot(P[i,:],Q[:,j]), 2) + (beta/2) * (pow(P[i][k],2) + pow(Q[k][j],2)) 
               (+ (pow (- (sel R :rows i :cols j) (mmult (sel P :rows i) (sel Q :cols j))) 2) (sum (count-e-beta1 P Q beta K)))
               0))))))

;(+ (pow (- (sel R :rows i :cols j) (mmult (sel P :rows i) (sel Q :cols j))) 2) (sum (count-e-beta1 P Q beta K)))

;update result, method for calculating update for one step
(defn update-result [mapa]
  (let [P (:P mapa)
        Q (:Q mapa)
        E (:E mapa)
        alpha (:alpha mapa)
        beta (:beta mapa)
        R (:R mapa)
        K (:K mapa)
        Eij (get-Eij R P Q)]
    
    (assoc mapa :P (calculate-P mapa Eij) 
                :Q (calculate-Q mapa Eij) 
                :E (conj E (calculate-error1 R P Q beta alpha K)))))

;final method calculate-all
;calculates final value for R~ matrix
(defn calculate-all [mapa step]
  (if (zero? step)
    mapa
    (recur (update-result mapa) (dec step))))

(defn get-movies-recommendation [user all-movies all-users matrix] 
   (reduce (fn [result movie] (assoc result movie (nth (sel matrix :rows (.indexOf all-users user)) (.indexOf all-movies movie)))) {} all-movies))

(defn get-recommendations [user critics-map all-movies all-users matrix]
  (sort-by #(val %) > (reduce (fn [mapa entry] (dissoc mapa (key entry))) (get-movies-recommendation user all-movies all-users matrix) (critics-map user))))

(defn matrix-factorization [critics-map user K] 
  (let [all-movies (get-all-movies critics-map)
        all-users (get-users critics-map)
        R (create-R-matrix critics-map all-movies all-users)
        mapa (init-values 0.0002 0.02 K (count (sel R :cols 0)) (count (sel R :rows 0) ) R)
        result (calculate-all mapa 5000)
        R-prim-matrix (mmult (:P result) (:Q result))]
  (get-recommendations user critics-map all-movies all-users R-prim-matrix)))


(defn error-vals [error-list] (reduce (fn [mapa entry] (conj mapa entry)) [] error-list))
(defn error-keys [error-list] (reduce (fn [mapa entry] (conj mapa (.toString (.indexOf error-list entry)))) [] error-list))

(view (line-chart (error-keys (:E result)) (error-vals (:E result)) :title "Matrix Factorization Error Chart" :y-label "Error Value" :x-label "Cycle"  ))
