(ns shadow-maps.core)

(deftype city-graph
  [nodes edges]
  )

(defrecord Location [loc-name loc-geo]) ;(Location. "Tel Baruch BCH" [200 200])
(defrecord Path [locations params])

;
; Data
;

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






