(ns airdish.degree
  (:require [ogre.core :as q])) 

(defn degree-of 
  "Returns the degree of the vertex or vertices. An optional function may be
passed in to indicate how to traverse and filter the neighbors of the
nodes."
  ([vtx] (degree-of vtx q/both))
  ([vtx trv]
     (q/query vtx
              trv
              q/count!)))

(defn degrees-of 
  ([vtxs] (degrees-of vtxs q/both))
  ([vtxs trv]
     (map #(degree-of % trv) vtxs)))

(defn degree-sequence
  ([vtxs] (degree-sequence vtxs q/both))
  ([vtxs trv] (sort > (degrees-of vtxs trv))))

(defn max-degree
  "Returns the maximum degree of the given graph."
  ([vtxs] (max-degree vtxs q/both))
  ([vtxs trv] (apply max (degrees-of vtxs trv))))

(defn min-degree
  "Returns the maximum degree of the given graph."
  ([vtxs] (min-degree vtxs q/both))
  ([vtxs trv] (apply min (degrees-of vtxs trv))))


