(ns shadow-maps.core)

(deftype city-graph
  [nodes edges]
  )

(print (pprint (search "Azrieli JCT" "Masaryk SQR")))



(def toy-search (make-search-function toy-dataset distance shadow-cost))

(toy-search "Azrieli JCT" "Masaryk SQR")

