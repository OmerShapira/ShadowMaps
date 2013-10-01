; Shadow Maps
; 2013, Omer Shapira and Nitzan Bartov
; Using an implementation of A* http://en.wikipedia.org/wiki/A*_search_algorithm
;

(ns shadow-maps.core
  (:use [clojure.data.priority-map])
  )

;
; Metric
;

(defn shadow-cost [edge] (:sun   (:params edge)))
(defn noise-cost  [edge] (:noise (:params edge)))

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
     (Math/sqrt (reduce + (map #(let [c (- %1 %2)] (* c c)) a b)))
   ))

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
                          (reduce + d  ; distance * (1 + sum of every cost)
                                   (map #(* (% edge) d) heuristics)
                                   )))

        search-dataset (let [paths (second dataset)
                              edges (map (fn
                                           [{locs :locations _ :params :as edge}]
                                           [(reduce into (map #(vector (:loc-geo %)) locs)) (total-cost edge)]
                                           )
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
