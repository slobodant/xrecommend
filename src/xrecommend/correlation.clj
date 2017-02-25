(ns xrecommend.correlation
  (:use xrecommend.parser))
(use 'clojure.contrib.math)

;DEFINE CRITICS MAP FOR LATER USAGE
;(def critics-map (get-key-vals-data-2 data))

;(def critics { :slobodan {:Film1 1 :Film2 4.5 :Film3 4.8}  :jovan {:Film1 4 :Film2 5.5 :Film3 6.8}} )
; Eucledian distance formula
;(/ 1 (+ 1  (sqrt (+ (expt 1 2) (expt 3 2)))))

;DEFINE POWER 2
(defn power2 [number]
  (Math/pow number 2))

;SUM OF SQUARES FOR 2 ELEMENTS
(defn sum-squares [mapa1 mapa2]
  (reduce (fn [r [f o]]
    (if (mapa2 f)
      (assoc r f (power2 (- o (mapa2 f)))) r)) {} mapa1))

;EUCLEDIAN DISTANCE FOR TWO ELEMENTS
(defn eucledian-dist [values-map]
  (if (= (count values-map) 0)
    0
    (/ 1 (+ 1 (reduce + (map val values-map))))))

;EUCLEDIAN DISTANCE END

;PEARSON COORRELATION SCORE
; (sum-product - sum-x-y/N) / sqrt ((sumX2 - sumX2/n)(sumY2 - sumY2/N))

(defn sum-map [map1 common]
  (reduce (fn [r f] (+ r (map1 f))) 0 common))

(defn sum-product [map1 map2 common]
  (reduce (fn [r f]
    (+ r (* (map1 f) (map2 f)))) 0 common))

(defn sum-squares-pearson [map1 common]
  (reduce (fn [r f] (+ r (Math/pow (map1 f) 2))) 0 common))

; function for calculating pearson correlation score between two users
(defn pearson-correlation [map1 map2]
  (let [common (filter map1 (keys map2))]
    (if (= 0 (count common))
      0
      (let [sum-maps (* (sum-map map1 common) (sum-map map2 common))
            sum-product-val (sum-product map1 map2 common)
            sum-squares-map1 (sum-squares-pearson map1 common)
            sum-squares-map2 (sum-squares-pearson map2 common)
            common-count (count common)
            numer (- sum-product-val (/ sum-maps common-count))
            denumer (Math/sqrt
          (* (- sum-squares-map1 (/ (Math/pow (sum-map map1 common) 2) common-count))
            (- sum-squares-map2 (/ (Math/pow (sum-map map2 common) 2) common-count))))]
        (if (= 0 denumer)
          0
          (double (/ numer denumer)))))))

;PEARSON COORRELATION SCORE END


;usage example
(eucledian-dist (sum-squares (critics "Lisa Rose") (critics "Gene Seymour")))

;final calculation
;(sort (for [key (keys critics)] (eucledian-dist (sum-squares (critics "Toby") (critics key)))))  

(defn calculate-dist-for-user [username critics-map algorithm]
  (for [key (keys critics-map)]
    (vector key (if (= "eucledian" algorithm)
      (eucledian-dist (sum-squares (critics-map username) (critics-map key)))
      (pearson-correlation (critics-map username) (critics-map key))))))



;CONVERTING CALC RESULTS TO MAP FOR EASIER COUNTING LAGER
(defn to-map2 [mapa lista]
  (assoc mapa (get lista 0) (get lista 1)))

(defn final-map-for-user [username critics-map algorithm]
  (reduce to-map2 {} (apply vector (calculate-dist-for-user username critics-map algorithm))))


(defn my-filter [input-map number]
  (filter #(> (val input-map)) number))

;GET N POSSIBLE SIMILAR USERS
(defn get-n-results [input-map num-results]
  (take num-results (sort-by #(val %) > input-map)))

;usage: (get-n-results (final-map-for-user "Toby" critics) 10)

;CALCULATE FINAL SCORE FOR MOVIES movievalue * eucledian-distance-value

(defn calculate-final-score-movies [mapa skalar]
  (reduce (fn [result entry] (assoc result (key entry) (* skalar (val entry)))) {} mapa))

;MULTIPLYING RESULTS
(defn multiply-result [final-result-map critics-map]
  (reduce (fn [result entry] (assoc result (key entry) (calculate-final-score-movies (critics-map (key entry)) (val entry)))) {} final-result-map))

;SUMMARING TWO MAPS AND ITS ALL VALUES
(defn sum-two-map-values [mapa1 mapa2]
  (reduce (fn [r [f o]]
    (if (nil? (mapa1 f))
      (assoc r f o)
      (assoc r f (+ (mapa1 f) o)))) mapa1 mapa2))


;TODO Filter all non users movies 
;(take 10 (sort-by #(val %) > all-movies-results))



;PROGRAM USAGE

;(def final-result-map (get-n-results (final-map-for-user "43" critics-map) 10))
;(def multipliedmap (multiply-result final-result-map))

;FINAL RESULTS FOR USER
;(def all-results (map val multipliedmap))

;FINAL 10 RESULTS FOR USER
;(def all-movies-results (reduce sum-two-map-values {} all-results))

;(defn dissoc-results [result user-critics-entry] 
;  (dissoc result (key user-critics-entry)))

;(defn filter-non-user-results [user-critics] (reduce dissoc-results all-movies-results user-critics))

;(take 10 (sort-by #(val %) > (filter-non-user-results (critics-map "43"))))


(defn calculate-dist-for-user-final [username number number-of-users algorithm critics-map]
  (def final-result-map (get-n-results (final-map-for-user username critics-map algorithm) number-of-users))
  (def multipliedmap (multiply-result final-result-map critics-map))
  (def all-results (map val multipliedmap))
  (def all-movies-results (reduce sum-two-map-values {} all-results))
  (take number (sort-by #(val %) > (reduce (fn [result user-critics-entry]
    (dissoc result (key user-critics-entry))) all-movies-results (critics-map username)))))

(def pearson-result (calculate-dist-for-user-final "Toby" 10 500 "pearson" critics))
(def eucledian-result (calculate-dist-for-user-final "Toby" 10 500 "eucledian" critics))

