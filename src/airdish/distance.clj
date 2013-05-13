(ns airdish.distance
  (:require [ogre.core :as q]
            [airdish.search :as search]))

(defn all-shortest-paths
  "Given a starting vertex, an ending vertex, and a maximum length,
conducts an iterative deepening depth first search for a path up to
the maximum length. Optionally, a subquery can be supplied to indicate
how to traverse the graph."
  ([vtx1 vtx2] (all-shortest-paths vtx1 vtx2 q/both))
  ([vtx1 vtx2 trv]
     (if (not= (.getId vtx1) (.getId vtx2))       
       (search/IDDFS vtx1 #(q/has % :id (.getId vtx2)) trv)
       [[]])))

(defn distance 
  "Given two vertices and a maximum distance, returns the distance
between the two vertices or Infinity if no path can be found that is
less than the maximum distance supplied."
  ([vtx1 vtx2]
     (distance vtx1 vtx2 q/both))
  ([vtx1 vtx2 trv]
     (let [paths (all-shortest-paths vtx1 vtx2 trv)
           rep   (first paths)]
       (cond 
        (empty? paths) Double/POSITIVE_INFINITY
        (empty? rep)   0
        :else (dec (count rep))))))

;;The rest of this is way inefficent. There are better algorithms for
;;all of this. 
(defn eccentricity 
  ([vtx vtxs] (eccentricity vtx vtxs q/both))
  ([vtx vtxs trv]
     (apply max (map #(distance vtx % trv) vtxs))))

(defn radius
  ([vtxs] (radius vtxs q/both))
  ([vtxs trv]
     (->> vtxs
          (map #(eccentricity % vtxs trv))
          (apply min))))

(defn diameter
  ([vtxs] (diameter vtxs q/both))
  ([vtxs trv]
     (->> vtxs
          (map #(eccentricity % vtxs trv))
          (apply max))))

(defn central-vertices
  ([vtxs] (central-vertices vtxs q/both))
  ([vtxs trv]
     (let [vtxs-eccs (map (juxt identity #(eccentricity % vtxs trv)) vtxs)
           radius  (apply min (map second vtxs-eccs))]
       (->> vtxs-eccs
            (filter #(= radius (second %)))
            (map first)))))

(defn peripheral-vertices 
  ([vtxs] (peripheral-vertices vtxs q/both))
  ([vtxs trv]
     (let [vtxs-eccs (map (juxt identity #(eccentricity % vtxs trv)) vtxs)
           diameter  (apply max (map second vtxs-eccs))]
       (->> vtxs-eccs
            (filter #(= diameter (second %)))
            (map first)))))