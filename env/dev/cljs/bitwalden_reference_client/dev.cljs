(ns ^:figwheel-no-load bitwalden-reference-client.dev
  (:require
    [bitwalden-reference-client.core :as core]
    [devtools.core :as devtools]))


(enable-console-print!)

(devtools/install!)

(core/init!)
