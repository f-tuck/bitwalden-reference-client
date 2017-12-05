(ns bitwalden-reference-client.prod
  (:require
    [bitwalden-reference-client.core :as core]))

;;ignore println statements in prod
(set! *print-fn* (fn [& _]))

(core/init!)
