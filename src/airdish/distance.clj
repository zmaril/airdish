(ns airdish.distance
  (:require [ogre.core :as q]
            [airdish.search :as search]))

(defn all-shortest-paths
  "Given a starting vertex, an ending vertex, and a maximum length
a-shortest-path conducts a iteartive deepening depth first search for
a path up to the maximum length. Optionally, a subquery can be
supplied to indicate how to traverse the graph."
  ([vtx1 vtx2 max] (all-shortest-paths vtx1 vtx2 max q/both))
  ([vtx1 vtx2 max trv]
     (search/IDDFS vtx1 max #(q/has % :id (.getId vtx2)) trv)))

(defn distance 
  "Given two vertices and a maximum distance, returns the distance
between the two vertices (or 0 if no path can be found that is less
than the maximum distance supplied."
  ([vtx1 vtx2 max]
     (distance vtx1 vtx2 max q/both))
  ([vtx1 vtx2 max trv]
     (count (first (all-shortest-paths vtx1 vtx2 max trv)))))

;; (defn radius [])

;; (defn lightest-path 
;;   "Returns a list containing the elements "
;;   ([vtx1 vtx2 wgt] 
;;      (shorted-path vtx1 vtx2 wgt q/both))
;;   ([vtx1 vtx2 trv wgt]));;Implement a simple lightest path algorithm
;; ;;here
