; Shadow Maps
; 2013, Omer Shapira and Nitzan Bartov

(ns shadow-maps.core
  (:use [clojure.data.priority-map])
  )


;
; Data
;

(defrecord Location [loc-name loc-geo]) ;(Location. "Tel Baruch BCH" [200 200])
(defrecord Path [locations params])

(let [
     ; Creating a closure and hiding the variable names
     rabin (Location. "Rabin SQR" [100 100])
     masrk (Location. "Masaryk SQR" [85 90])
     basel (Location. "Basel CPX" [85 160])
     arloz (Location. "Arlozorov CBS" [140 140])
     magen (Location. "Magen David SQR" [60 0])
     azrli (Location. "Azrieli JCT" [160 0])]
  (def toy-dataset
  "[Vertices, Edges]
  Done using hash-sets for pairwise equality"
  [
   ; Vertices
   #{rabin masrk basel arloz magen azrli}
   ; Weighted Edges
   #{
     (Path. #{rabin masrk} {:sun 0.9 :noise 0.6})
     (Path. #{rabin arloz} {:sun 0.6 :noise 0.4})
     (Path. #{rabin basel} {:sun 0.4 :noise 0.35})
     (Path. #{masrk magen} {:sun 0.8 :noise 0.85})
     (Path. #{arloz azrli} {:sun 1.0 :noise 0.95})
     (Path. #{magen azrli} {:sun 0.8 :noise 0.9})
    }
  ])
 )


;
; Metric
;


(defn shadow-cost
  "Returns the inconvenience cost of walking in the sun"
  [edge]
  (:sun (:params edge))
  )


(defn distance
  "Returns the distance on the map trail between begin and end"
  ([edge]
  (let
    [{locs :locations _ :params} edge
     loc-a (:loc-geo(first locs))
     loc-b (:loc-geo (second locs))
     square #(* % %)
     len (reduce (fn [[a b] [c d]] [(+ a c) (+ b d)]) [loc-a loc-b])
     ]
    (Math/sqrt (reduce + 0 (map square len)))))

  ([a b] ; TODO: Unify
   (Math/sqrt (reduce + (map #(let [c (- %1 %2)] (* c c)) a b))))
  )

(defn total-cost
  "Heuristic total cost function"
  [edge]
  (let
    [d (distance edge)
     s (shadow-cost edge)]
    (* d (+ 1 (* 1 s))) ;walking in the sun is twice as expensive
    )
  )

;
; Metric & Data Tests
;
(let
  [e (first (first(rest toy-dataset)))]
  (total-cost e)
  )


;
; Data translation to an efficient A*
;
(def edge-list
  (let [paths (second toy-dataset)
        edges (map (fn [{locs :locations _ :params :as edge}]
           [(reduce into (map #(vector (:loc-geo %)) locs)) (total-cost edge)]) paths )]
    (apply hash-map(reduce into edges))
    )
  )

(defn name-to-coord [dataset n] (:loc-geo (first (filter #(= (:loc-name %) n) (first dataset)))))
(defn coord-to-name [dataset c] (:loc-name (first (filter #(= (:loc-geo %) c) (first dataset)))))

;
; Algorithm
;


(defn A*
 "Finds a path between start and goal inside the graph described by edges
  (a map of edge to distance); estimate is an heuristic for the actual
  distance. Accepts a named option: :monotonic (default to true).
  Returns the path if found or nil.

  http://clj-me.cgrand.net/2010/09/04/a-in-clojure/
  "

 [edges estimate start goal & {mono :monotonic :or {mono true}}]
  (let [f (memoize #(estimate % goal))
        neighbours (reduce (fn [m [a b]] (assoc m a (conj (m a #{}) b)))
                      {} (keys edges))]
    (loop [q (priority-map start (f start))
           preds {}
           shortest {start 0}
           done #{}]
      (when-let [[x hx] (peek q)]
        (if (= goal x)
          (reverse (take-while identity (iterate preds goal)))
          (let [dx (- hx (f x))
                bn (for [n (remove done (neighbours x))
                         :let [hn (+ dx (edges [x n]) (f n))
                               sn (shortest n Double/POSITIVE_INFINITY)]
                         :when (< hn sn)]
                     [n hn])]
            (recur (into (pop q) bn)
              (into preds (for [[n] bn] [n x]))
              (into shortest bn)
              (if mono (conj done x) done))))))))

;
; Algorithm Wrapper for Execution
;


(defn search
  "Takes in names from the toy dataset and outputs the least sunny route between them, if one exists"
  [from to]
  (let [from-coord (name-to-coord toy-dataset from)
        to-coord (name-to-coord toy-dataset to)]
    (map #(coord-to-name toy-dataset %)(A* edge-list distance from-coord to-coord))
    ))
