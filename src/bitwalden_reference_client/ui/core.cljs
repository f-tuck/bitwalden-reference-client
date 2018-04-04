(ns bitwalden-reference-client.ui.core
    (:require
      [reagent.core :as r]
      [bitwalden-reference-client.core :as client]))

(defn mount-root [& args]
  (apply client/mount-root args))

(defn init! []
  (mount-root))
