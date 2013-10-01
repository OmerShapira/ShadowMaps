(ns shadow-maps.core)

(def toy-search (make-search-function toy-dataset distance shadow-cost))

(toy-search "Azrieli JCT" "Masaryk SQR")
