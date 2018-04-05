(ns bitwalden-reference-client.ui.processing
  (:require [clojure.set :refer [difference]]
            [clojure.string :refer [join]]))

(defn proc+! [account id]
  (swap! account update-in [:state :processing] #(into #{id} %)))

(defn proc-! [account id]
  (swap! account update-in [:state :processing] difference #{id}))

(defn unpack-processing [account-data]
  (let [processing-items (get-in account-data [:state :processing])
        processing? (> (count processing-items) 0)
        processing-txt (join "\n" processing-items)]
    {:items processing-items
     :active? processing?
     :text processing-txt}))

(defn component-processing-indicator [account-data]
  (let [processing (unpack-processing account-data)]
    [:span#indicator.spinner {:title (processing :text) :alt (processing :text)}]))

