(ns itmo-fp-lab3.core
  (:require
   [clojure.tools.cli :refer [parse-opts]]
   [clojure.core.async :as async]))

(defn grid-points [l r step]
  (map #(* step %) (range (long (Math/ceil (/ l step)))
                           (inc (long (Math/floor (/ r step)))))))

(defn linear-interpolation [points step l r]
  (let [[p1 p2] points
        x1 (:x p1) y1 (:y p1)
        x2 (:x p2) y2 (:y p2)]
    (map (fn [x]
           {:x (double x)
            :y (+ y1 (* (/ (- x x1) (- x2 x1)) (- y2 y1)))})
         (grid-points l r step))))

(defn lagrange-polynomial [points x]
  (reduce + (map-indexed
             (fn [i pi]
               (* (:y pi)
                  (reduce * (map-indexed
                             (fn [j pj]
                               (if (= i j) 1.0
                                   (/ (- x (:x pj))
                                      (- (:x pi) (:x pj)))))
                             points))))
             points)))

(defn -main [& args]
  (println "Interpolation algorithms ready"))
