; Shadow Maps
; 2013, Omer Shapira and Nitzan Bartov
; Using an implementation of A* http://en.wikipedia.org/wiki/A*_search_algorithm
;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
     ]
    (distance loc-a loc-b)))

  ([a b]
   (Math/sqrt (reduce + (map #(let [c (- %1 %2)] (* c c)) a b))))
  )


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

  ;FIXME: These are terribly inneficient
(defn name-to-coord [dataset n] (:loc-geo (first (filter #(= (:loc-name %) n) (first dataset)))))
(defn coord-to-name [dataset c] (:loc-name (first (filter #(= (:loc-geo %) c) (first dataset)))))


(defn make-search-function
  "Make a search function with a closure over the dataset.
  Still expensive."

  [dataset distance & heuristics]

  (let [total-cost (fn [edge]
                      (let [d (distance edge)]
                        (reduce + d  ;sum over
                                 (map #(* (% edge) d) heuristics) ;the application of each function on edge, times the distance
                                 )))

        search-dataset (let [paths (second dataset)
                              edges (map (fn [{locs :locations _ :params :as edge}]
                                             [(reduce into (map #(vector (:loc-geo %)) locs))
                                              (total-cost edge)
                                              ])
                                         paths)
                              ]
                          (apply hash-map (reduce into edges))
                          )
        ]

    (fn [from to]
      (let [from-coord (name-to-coord dataset from)
            to-coord (name-to-coord dataset to)
            ]
           (map #(coord-to-name dataset %)
                (A* search-dataset distance from-coord to-coord)
                ))
    )))



(defn pprint
  ;FIXME : This cookie tastes funky
  "Pretty-prints the path"
  [path]
  (let [text (fn [x y]
               (if (= y "") (str "Reach your destination at" x) (str "Go to " x "\n" y))
               )]
    (reduce text "" path)
    )
  )





