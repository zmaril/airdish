(ns airdish.distance-test
  (:require [ogre.core         :as q]
            [airdish.distance  :as ad]
            [archimedes.core   :as tg]
            [archimedes.vertex :as tv]
            [clojure.test :refer :all]))

(defn path-namer [lst]
  (map (partial map (q/prop :name)) lst))

(deftest all-shortest-paths-test
  (let [hercules (first (tv/find-by-kv :name "hercules"))
        neptune  (first (tv/find-by-kv :name "neptune"))
        pluto    (first (tv/find-by-kv :name "pluto"))]
    (is (= (path-namer (ad/all-shortest-paths hercules neptune 100))
           '(("hercules" "jupiter" "neptune"))))
    (is (= (path-namer (ad/all-shortest-paths hercules neptune 100
                                             #(q/both % [:battled :pet :brother])))
           '(("hercules" "cerberus" "pluto" "neptune"))))
    (is (= (path-namer (ad/all-shortest-paths hercules neptune 3
                                             #(q/both % [:battled :pet :brother])))
           '()))
    (is (= (ad/all-shortest-paths hercules neptune 0
                                  #(q/both % [:battled :pet :brother]))
           '()))
    (is (= (path-namer (ad/all-shortest-paths hercules pluto 3))
           '(("hercules" "cerberus" "pluto") ("hercules" "jupiter" "pluto"))))
    (is (= (path-namer (ad/all-shortest-paths hercules pluto 3 
                                              (q/subquery 
                                               q/both
                                               (q/has-not :type "monster"))))
           '(("hercules" "jupiter" "pluto"))))))

(deftest distance-test
  (let [hercules (first (tv/find-by-kv :name "hercules"))
        neptune  (first (tv/find-by-kv :name "neptune"))
        pluto    (first (tv/find-by-kv :name "pluto"))]
    (is (= 3 (ad/distance hercules neptune 100)))
    (is (= 4 (ad/distance hercules neptune 100 #(q/both % [:battled :pet :brother]))))
    (is (= 0 (ad/distance hercules neptune 3   #(q/both % [:battled :pet :brother]))))
    (is (= 3 (ad/distance hercules pluto 3)))
    (is (= 3 (ad/distance hercules pluto 3
                          (q/subquery 
                           q/both
                           (q/has-not :type "monster")))))))
