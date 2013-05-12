(ns airdish.core-test
  (:require [archimedes.core :as tg]
            [archimedes.io   :as tio])
  (:use clojure.test))

(tg/use-clean-graph!)
;;https://github.com/thinkaurelius/titan/wiki/Getting-Started#the-graph-of-the-gods
(tio/load-graph-graphson "data/titan_graph.graphson")
