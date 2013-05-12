(ns airdish.degree-test
  (:require [ogre.core         :as q]
            [airdish.degree    :as ad]
            [archimedes.core   :as tg]
            [archimedes.vertex :as tv]
            [archimedes.vertex :as te]
            [clojure.test :refer :all]))

;;TODO: This should be in ogre.
(defmacro subquery 
  ""
  [& body]
  `(fn [p#]
     (-> p#
         ~@body)))

(deftest degree-of-test
  (let [pluto (first (tv/find-by-kv :name "pluto"))]
    (testing "Default/both traversals"
      (is (= (ad/degree-of pluto) 
             6))
      (is (= (ad/degree-of pluto q/both) 
             6))
      (is (= (ad/degree-of pluto 
                           (subquery (q/both [:brother]))) 
             4))
      (is (= (ad/degree-of pluto 
                           (subquery (q/both [:brother :pet]))) 
             5))
      (is (= (ad/degree-of pluto 
                           (subquery (q/both [:brother :pet :lives]))) 
             6))
      (is (= (ad/degree-of pluto 
                           (subquery (q/both [:wrong_label]))) 
             0)))

    (testing "Out traversals"
      (is (= (ad/degree-of pluto q/out) 
             4))
      (is (= (ad/degree-of pluto 
                           (subquery (q/out [:pet]))) 
             1))
      (is (= (ad/degree-of pluto 
                           (subquery (q/out [:pet :lives]))) 
             2))
      (is (= (ad/degree-of pluto 
                           (subquery (q/out [:pet :lives :brother]))) 
             4))
      (is (= (ad/degree-of pluto 
                           (subquery (q/out [:wrong_label]))) 
             0)))

    (testing "In traversals"
      (is (= (ad/degree-of pluto
                           q/in)
             2))
      (is (= (ad/degree-of pluto 
                           (subquery (q/in [:brother])))))
      (is (= (ad/degree-of pluto 
                           (subquery (q/in [:wrong_label]))) 
             0)))
    
    (testing "Subgraph inducing"
      (let [god-neighbors    (subquery
                              q/-->
                              (q/has :type "god"))
            brothers-homes (subquery 
                            (q/--> [:brother])
                            (q/--> [:lives]))]
        (is (= (ad/degree-of pluto god-neighbors) 2))      
        (is (= (ad/degree-of pluto brothers-homes) 2))))))  

  (testing "Total degrees"
    (let [vertices  (.getVertices (tg/get-graph))]
      (is (= (ad/degree-of vertices)
             (* 2 17)))
      (is (= (ad/degree-of vertices
                           (subquery q/<E> (q/has :time = nil)))
             (* 2 14)))
      (is (= (ad/degree-of vertices
                           (subquery q/<E> (q/has :time = (int 0))))
             (* 2 0)))
      (is (= (ad/degree-of vertices
                           (subquery q/<E> (q/has :time = (int 1))))
             (* 2 1)))
      (is (= (ad/degree-of vertices
                           (subquery q/<E> (q/has :time = (int 2))))
             (* 2 1)))
      (is (= (ad/degree-of vertices
                           (subquery q/<E> (q/has :time >= (int 3))))
             (* 2 1)))))


(deftest degree-sequence-test
  (is (= '(7 6 5 5 3 2 1 1 1 1 1 1) 
         (ad/degree-sequence (tv/get-all-vertices))))
  (is (= '(5 4 4 3 1 0 0 0 0 0 0 0) 
         (ad/degree-sequence (tv/get-all-vertices)
                             q/-->)))
  (is (= '(3 2 2 2 2 1 1 1 1 1 1 0) 
         (ad/degree-sequence (tv/get-all-vertices)
                             q/<--))))

(deftest max-degree-test
  (let [vtxs (tv/get-all-vertices)]
    (is (= 7 (ad/max-degree vtxs)))
    (is (= 5 (ad/max-degree vtxs (subquery (q/-->)))))
    (is (= 3 (ad/max-degree vtxs (subquery (q/<--)))))
    (is (= 4 (ad/max-degree vtxs (subquery (q/<-> [:brother])))))))

(deftest max-degree-test
  (let [vtxs (tv/get-all-vertices)]
    (is (= 1 (ad/min-degree vtxs)))
    (is (= 0 (ad/min-degree vtxs (subquery (q/-->)))))
    (is (= 0 (ad/min-degree vtxs (subquery (q/<--)))))
    (is (= 0 (ad/min-degree vtxs (subquery (q/<-> [:brother])))))))
