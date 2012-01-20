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

(defn calculate-Eij [mapa i j] 
  (- (sel (:R mapa) :rows i :cols j) (mmult (sel (:P mapa) :rows i) (sel (:Q mapa) :cols j))))

; e is number not matrix! need to calculate it properly
(defn calculate-E [full-map]
  (matrix (for [i (range 0 (count (:R full-map)))]
                 (for [j (range 0 (count (sel (:R full-map) :rows i)))] 
                   (if (> (sel (:R full-map) :rows i :cols j) 0)
                     (- (sel (:R full-map) :rows i :cols j) (mmult (sel (:P full-map) :rows i) (sel (:Q full-map) :cols j))) 0)))))


;P calculating finished!
; add calculating
(defn calculate-P [full-map]
  (reduce plus (map matrix 
                 (for [j (range 0 (count (sel (:R full-map) :rows 0)))]
                   (for [i (range 0 (count (:R full-map)))]
                     (for [k (range 0 (:K full-map))]
                       (if (> (sel (:R full-map) :rows i :cols j) 0)
                         (+ (sel (:P full-map) :rows i :cols k) (* (:alpha full-map) (- (* 2 (calculate-Eij full-map i j) (sel (:Q full-map) :rows k :cols j)) (* (:beta full-map) (sel (:P full-map) :rows i :cols k)))))
                         0)))))))


;Q calculating finished
(defn calculate-Q [full-map]
  (trans (reduce plus (map matrix
                      (for [i (range 0 (count (:R full-map)))]
                        (for [j (range 0 (count (sel (:R full-map) :rows i)))]
                          (for [k (range 0 (:K full-map))]
                            (if (> (sel (:R full-map) :rows i :cols j) 0)
                              (+ (sel (:Q full-map) :rows k :cols j ) (* (:alpha full-map) (- (* 2 (calculate-Eij full-map i j) (sel (:P full-map) :rows i :cols k)) (* (:beta full-map) (sel (:Q full-map) :rows k :cols j)))))
                              0))))))))

;init values for calculating
(def R (matrix [[5,3,0,1],
     [4,0,0,1],
     [1,1,0,5],
     [1,0,0,4],
     [0,1,5,4]]))


; example: 
; 
(defn init-values [alpha beta1 K rows columns R] 
  (def mapa {})
  (assoc mapa :alpha alpha :beta beta1 :K K :P (random-matrix K rows) :Q (random-matrix columns K) :E (matrix 0 rows columns) :R R))

(def mapa (init-values 0.0002 0.02 2 5 4 R)) 

;calculating small "e"
(defn count-eB [P Q beta1] 
  (reduce (fn [res row] 
          (* (/ beta1 2) (+ (pow (sum (sel (trans P) :rows row)) 2) (pow (sum (sel Q :rows row)) 2)))) (range 0 (:K mapa))))

;update result, method for calculating update for one step
(defn update-result [mapa]
  (let [P (:P mapa)
        Q (:Q mapa)
        E (:E mapa)
        alpha (:alpha mapa)
        beta1 (:beta mapa)
        R (:R mapa)]
    (assoc mapa :P (calculate-P mapa) 
                :Q (calculate-Q mapa) 
                :E (plus E (pow (minus R (mmult P Q))) (matrix (count-eB P Q beta1) (count (sel R :cols 0)) (count (sel R :rows 0)))))))
;final method calculate-all 
;calculates final value for R' matrix
(defn calculate-all [mapa step]
  (if (zero? step)
    mapa
    (recur (update-result mapa) (dec step))))

; Not getting good results!!!! CHECK THIS AS SOON AS POSIBLE!!!!!
; example (calculate-all mapa 60)
; TODO check why calculating does not work as expected!
; HINT P and Q calculating issue!!!

