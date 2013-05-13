(ns airdish.distance-test
  (:require [ogre.core         :as q]
            [airdish.search    :as search]
            [airdish.distance  :as ad]
            [archimedes.core   :as tg]
            [archimedes.vertex :as tv]
            [clojure.test :refer :all]))

(def infinity Double/POSITIVE_INFINITY)

(defn path-namer [lst]
  (map (partial map (q/prop :name)) lst))

(deftest all-shortest-paths-test
  (let [hercules (first (tv/find-by-kv :name "hercules"))
        neptune  (first (tv/find-by-kv :name "neptune"))
        pluto    (first (tv/find-by-kv :name "pluto"))]
    (is (= (path-namer (ad/all-shortest-paths hercules neptune))
           '(("hercules" "jupiter" "neptune"))))
    (is (= (path-namer (ad/all-shortest-paths hercules neptune
                                              #(q/both % [:battled :pet :brother])))
           '(("hercules" "cerberus" "pluto" "neptune"))))
    (search/with-max-depth 3
      (is (= (path-namer (ad/all-shortest-paths hercules neptune
                                                #(q/both % [:battled :pet :brother])))
             '()))
      (is (= (ad/all-shortest-paths hercules neptune
                                    #(q/both % [:battled :pet :brother]))
             '())))
    (is (= (path-namer (ad/all-shortest-paths hercules pluto))
           '(("hercules" "cerberus" "pluto") ("hercules" "jupiter" "pluto"))))
    (is (= (path-namer (ad/all-shortest-paths hercules pluto
                                              (q/subquery 
                                               q/both
                                               (q/has-not :type "monster"))))
           '(("hercules" "jupiter" "pluto"))))))

(deftest distance-test
  (let [hercules (first (tv/find-by-kv :name "hercules"))
        neptune  (first (tv/find-by-kv :name "neptune"))
        pluto    (first (tv/find-by-kv :name "pluto"))]
    (is (= 2  (ad/distance hercules neptune)))
    (is (= 0  (ad/distance hercules hercules)))
    (is (= 3  (ad/distance hercules neptune #(q/both % [:battled :pet :brother]))))
    (search/with-max-depth 3
      (is (= infinity (ad/distance hercules neptune #(q/both % [:battled :pet :brother]))))
      (is (= 2  (ad/distance hercules pluto)))
      (is (= 2  (ad/distance hercules pluto
                             (q/subquery 
                              q/both
                              (q/has-not :type "monster"))))))))

(deftest eccentricity-test
  (let [hercules (first (tv/find-by-kv :name "hercules"))
        neptune  (first (tv/find-by-kv :name "neptune"))
        pluto    (first (tv/find-by-kv :name "pluto"))]
        (is (= 3 (ad/eccentricity hercules (tv/get-all-vertices))))
        (is (= 3 (ad/eccentricity neptune  (tv/get-all-vertices))))
        (is (= 3 (ad/eccentricity pluto    (tv/get-all-vertices))))))

(deftest eccentricity-test
  (let [hercules (first (tv/find-by-kv :name "hercules"))
        neptune  (first (tv/find-by-kv :name "neptune"))
        pluto    (first (tv/find-by-kv :name "pluto"))
        sample #{hercules neptune pluto}]
        (is (= 3 (ad/eccentricity hercules (tv/get-all-vertices))))
        (is (= 3 (ad/eccentricity neptune  (tv/get-all-vertices))))
        (is (= 3 (ad/eccentricity pluto    (tv/get-all-vertices))))

        (is (= infinity (ad/eccentricity hercules (tv/get-all-vertices)  q/in)))
        (is (= infinity (ad/eccentricity neptune  (tv/get-all-vertices)  q/in)))
        (is (= infinity (ad/eccentricity pluto    (tv/get-all-vertices)  q/in)))

        (is (= 3 (ad/eccentricity hercules (tv/get-all-vertices)  q/out)))
        (is (= infinity
               (ad/eccentricity neptune  (tv/get-all-vertices)  q/out)))
        (is (= infinity
               (ad/eccentricity pluto    (tv/get-all-vertices)  q/out)))

        (is (= 2 (ad/eccentricity hercules sample )))
        (is (= 2 (ad/eccentricity neptune  sample )))
        (is (= 2 (ad/eccentricity pluto sample )))

        (is (= 2 (ad/eccentricity hercules sample  q/out)))
        (is (= infinity (ad/eccentricity neptune  sample  q/out)))
        (is (= infinity (ad/eccentricity pluto    sample  q/out)))

        (is (= infinity (ad/eccentricity hercules sample  q/in)))
        (is (= 2 (ad/eccentricity neptune  sample  q/in)))
        (is (= 2 (ad/eccentricity pluto    sample  q/in)))

        (is (= infinity
               (ad/eccentricity hercules (tv/get-all-vertices)  #(q/out % #{:wrong_label}))))))


(deftest radius-test
  (let [hercules (first (tv/find-by-kv :name "hercules"))
        neptune  (first (tv/find-by-kv :name "neptune"))
        pluto    (first (tv/find-by-kv :name "pluto"))
        sample #{hercules neptune pluto}]
        (is (= 2 (ad/radius (tv/get-all-vertices) )))
        (is (= infinity (ad/radius (tv/get-all-vertices) q/in)))
        (is (= 3 (ad/radius (tv/get-all-vertices) q/out)))
        (is (= 2 (ad/radius sample  q/out)))        
        (is (= infinity
               (ad/radius (tv/get-all-vertices)  #(q/out % #{:wrong_label}))))))

(deftest diameter-test
  (let [hercules (first (tv/find-by-kv :name "hercules"))
        neptune  (first (tv/find-by-kv :name "neptune"))
        pluto    (first (tv/find-by-kv :name "pluto"))
        sample #{hercules neptune pluto}]
        (is (= 4 (ad/diameter (tv/get-all-vertices) )))
        (is (= infinity (ad/diameter (tv/get-all-vertices)  q/in)))
        (is (= infinity (ad/diameter (tv/get-all-vertices)  q/out)))
        (is (= infinity (ad/diameter sample  q/out)))        
        (is (= infinity
               (ad/diameter (tv/get-all-vertices)  #(q/out % #{:wrong_label}))))))

(deftest central-vertices-test
  (let [sample (tv/find-by-kv :type "god")]
    (is (= '("jupiter") 
           (map (q/prop :name) (ad/central-vertices (tv/get-all-vertices)))))
    (is (= '("cerberus" "sea" "hercules" "nemean" 
             "tartarus" "jupiter" "alcmene" "saturn" 
             "sky" "hydra" "neptune" "pluto")
           (map (q/prop :name) (ad/central-vertices (tv/get-all-vertices) q/in))))
    (is (=  '("hercules") 
           (map (q/prop :name) (ad/central-vertices (tv/get-all-vertices) q/out))))

    (is (= '("jupiter" "neptune" "pluto") 
           (map (q/prop :name) (ad/central-vertices sample))))
    (is (= '("jupiter" "neptune" "pluto") 
           (map (q/prop :name) (ad/central-vertices sample q/in))))
    (is (= '("jupiter" "neptune" "pluto") 
           (map (q/prop :name) (ad/central-vertices sample q/out))))))

(deftest peripheral-vertices-test
  (let [sample (tv/find-by-kv :type "god")]
    (is (= '("sea" "nemean" "alcmene" "hydra") 
           (map (q/prop :name) (ad/peripheral-vertices (tv/get-all-vertices)))))
    (is (= '("cerberus" "sea" "hercules" "nemean" 
             "tartarus" "jupiter" "alcmene" "saturn" 
             "sky" "hydra" "neptune" "pluto")
           (map (q/prop :name) (ad/peripheral-vertices (tv/get-all-vertices) q/in))))
    (is (=  '("cerberus" "sea" "nemean" 
             "tartarus" "jupiter" "alcmene" "saturn" 
             "sky" "hydra" "neptune" "pluto")
           (map (q/prop :name) (ad/peripheral-vertices (tv/get-all-vertices) q/out))))

    (is (= '("jupiter" "neptune" "pluto") 
           (map (q/prop :name) (ad/peripheral-vertices sample))))
    (is (= '("jupiter" "neptune" "pluto") 
           (map (q/prop :name) (ad/peripheral-vertices sample q/in))))
    (is (= '("jupiter" "neptune" "pluto") 
           (map (q/prop :name) (ad/peripheral-vertices sample q/out))))))

