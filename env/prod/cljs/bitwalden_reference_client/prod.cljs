(ns bitwalden-reference-client.prod
  (:require
    [bitwalden-reference-client.ui.core :as core]))

;;ignore println statements in prod
(set! *print-fn* (fn [& _]))

(core/init!)
