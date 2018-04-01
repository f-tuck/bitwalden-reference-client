(ns ^:figwheel-no-load bitwalden-reference-client.dev
  (:require
    [bitwalden-reference-client.ui :as ui]
    [devtools.core :as devtools]))


(enable-console-print!)

(devtools/install!)

(ui/init!)
