(ns xrecommend.core
  (:gen-class)
  (:use (incanter core stats charts io datasets)))

; Sample data set provided by Incanter
(def data (read-dataset 
            "https://raw.github.com/liebke/incanter/master/data/iris.dat" 
            :delim \space 
            :header true))

(defn extract [f]
  (fn [data]
     (map #(apply f (sel data :cols %)) (range 0 (ncol data)))))

(defn fill [n row] (map (fn [x] row) (range 0 n)))

(defn matrix-row-operation [operand row matrix] 
  (operand matrix 
    (fill (nrow matrix) row)))

; Probably could be much nicer using `reduce`
(defn normalize [matrix]
  (let [shifted (matrix-row-operation minus ((extract min) matrix) matrix)]
   (matrix-row-operation div ((extract max) shifted) shifted)))

(def normalized-data
  (normalize (to-matrix (sel data :cols [0 1]))))

(def normalized-plot (scatter-plot
            (sel normalized-data :cols 0)
            (sel normalized-data :cols 1)
            :x-label "Sepal Length"
            :y-label "Sepal Width"
            :group-by (sel data :cols 4)))

(defn -main [& args]
  (view normalized-plot))