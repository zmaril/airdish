(ns airdish.search
  (:require [ogre.core :as q]))

(def ^{:dynamic true} *max-search-depth* 10)

(defmacro with-max-depth [d & body]
  `(binding [*max-search-depth* ~d]
     ~@body))

;; ;;This'll be tough to implement through gremlin.
;; (defn breadth-first-search 
;;   "Given a vertex and predicate, this function executes a breadth
;; first search for a vertex which matches the predicate. An optional
;; function can be provided that defines how to traverse and filter the
;; graph."
;;   ([vtx pred] (breadth-first-search vtx pred q/both))
;;   ([vtx pred trv] 
;;      (q/query vtx
;;               ;;breath first search 
;;               ;;queue
;; )))

;; (defn depth-first-search 
;;   "Given a vertex and predicate, this function executes a depth
;; first search for a vertex which matches the predicate. An optional
;; function can be provided that defines how to traverse and filter the
;; graph."
;;   ([vtx pred] (depth-first-search vtx pred q/both))
;;   ([vtx pred trv] 
;;      (q/query vtx
;;               ;;depth first search
;;               ;;stack
;; )))

(defn depth-limited-query
  ([vtx pred d] (depth-limited-query vtx pred d q/both))
  ([vtx pred d trv]     
     (let [here (str (java.util.UUID/randomUUID))]
       (q/query vtx
                (q/as here)
                trv
                (q/loop-to here (fn [l o p] (< l d)))                
                pred))))

(defn IDDFS
  "Given a vertex, a max depth, and a predicate, this function
 executes a iterative deepening search for the shortest paths to a
 vertex which matches the predicate (or until it hits the max depth).
 An optional function can be provided that defines how to traverse and
 filter the graph"
  ([vtx pred] (IDDFS vtx pred q/both))
  ([vtx pred trv] 
     (loop [d 0]
       (let [results (-> (depth-limited-query vtx pred d trv)
                         q/path
                         (q/dedup)
                         q/all-into-vecs!)]
         (if (and (empty? results) (< d *max-search-depth*))
           (recur (inc d))
           results)))))

(defn a*-search [])