(ns bitwalden-reference-client.ui.core
  (:require
    [reagent.core :as r]
    [cljs.core.async :refer [<!]]
    [bitwalden-reference-client.ui.views :refer [component-container]]
    [bitwalden-reference-client.core :as client])
  (:require-macros
    [cljs.core.async.macros :refer [go]]))

(defn mount-root [& args]
  (print "mount-root" args)
  (go
    (let [account (<! (client/initialize (nil? args)))]
      (r/render [component-container account] (.getElementById js/document "app")))))

(defn init! []
  (mount-root))
