(ns xrecommend.matrixFactorization
  (:gen-class)
  (:use (incanter core stats charts io datasets)))
;def matrix_factorization(R, P, Q, K, steps=5000, alpha=0.0002, beta=0.02):
;    Q = Q.T
;    for step in xrange(steps):
;    // DONE
;        for i in xrange(len(R)):
;            for j in xrange(len(R[i])):
;                if R[i][j] > 0:
;                    eij = R[i][j] - numpy.dot(P[i,:],Q[:,j])
;                    for k in xrange(K):
;                        P[i][k] = P[i][k] + alpha * (2 * eij * Q[k][j] - beta * P[i][k])
;                        Q[k][j] = Q[k][j] + alpha * (2 * eij * P[i][k] - beta * Q[k][j])
;        eR = numpy.dot(P,Q)
;     // END DONE
;     // TODO:
;        e = 0
;        for i in xrange(len(R)):
;            for j in xrange(len(R[i])):
;                if R[i][j] > 0:
;                    e = e + pow(R[i][j] - numpy.dot(P[i,:],Q[:,j]), 2)
;                    for k in xrange(K):
;                        e = e + (beta/2) * (pow(P[i][k],2) + pow(Q[k][j],2))
;       if e < 0.001:
;           break            
;return P, Q.T


; calculate P, Q and E probably recursive

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
(def R (matrix [[5,3,0,1],
     [4,0,0,1],
     [1,1,0,5],
     [1,0,0,4],
     [0,1,5,4]]))

(def mapa (init-values 0.0002 0.02 2 5 4 R))

(defn calculate-all [mapa step]
  (if (zero? step)
    mapa
    (recur (update-result mapa) (dec step))))


(def result (calculate-all mapa 5000))
(mmult (:P result) (:Q result))
