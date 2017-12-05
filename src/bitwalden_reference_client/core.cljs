(ns bitwalden-reference-client.core
  (:require
    [reagent.core :as r]
    [cljs.core.async :refer [<!]]
    [bitwalden-client-lib.core :as bitwalden]
    [bitwalden-client-lib.rpc :refer [refresh-known-nodes]]
    [bitwalden-client-lib.data :as accounts]
    [bitwalden-client-lib.crypto :refer [generate-keypair-seed-b58 keypair-from-seed-b58 public-key-b58-from-keypair]]
    [bitwalden-client-lib.util :refer [random-hex]])
  (:require-macros
    [cljs.core.async.macros :refer [go]]))

(def account-key "bitwalden-account")

(defonce nodes (r/atom []))

(defn load-account []
  (js->clj (js/JSON.parse (.getItem (aget js/window "localStorage") account-key))))

(defn save-account! [account-data]
  (.setItem (aget js/window "localStorage") account-key (js/JSON.stringify (clj->js account-data)))
  account-data)

(defonce account (r/atom (or (load-account) (accounts/make-empty-account))))

(defn create-new-key [account ev]
  (let [seed (generate-keypair-seed-b58)
        keypair (keypair-from-seed-b58 seed)
        pk (public-key-b58-from-keypair keypair)]
    (print seed keypair pk)
    (swap! account assoc "keys" {"seed" seed "public-key" pk} "feed" (accounts/make-json-feed pk) "profile" (accounts/make-profile pk))
    (save-account! @account)))

(defn post! [post-ui account content ev]
  (print "post!")
  (swap! post-ui assoc :state :posting)
  (go
    (let [profile (@account "profile")
          keypair (keypair-from-seed-b58 (get-in @account ["keys" "seed"]))
          post-struct (accounts/make-post (random-hex 32) content)
          node (rand-nth @nodes)
          _ (print "post-struct:" post-struct)
          _ (print "node:" node)
          post-response (<! (bitwalden/add-post! node keypair post-struct profile (@account "feed")))]
      (print "response:" post-response)
      (if (post-response "error")
        (swap! post-ui assoc :state :error :error (post-response "message"))
        (do
          (swap! account assoc "profile" (post-response :profile) "feed" (post-response :feed))
          (swap! post-ui assoc :state nil :post "" :error nil)
          (save-account! @account)))))
  (.preventDefault ev)
  (.blur (.. ev -target)))

;; -------------------------
;; Views

(defn component-demo-layout [account]
  [:div
   [:div.author "(see medium screenshot for author block)"]
   [:h2 "Some Title Goes Here"]
   [:p "Some paragraph. Hello. Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum."]
   [:img {:src "https://i.pinimg.com/736x/dc/b6/c5/dcb6c527b5655ef2582719445a8c925c--fan-theories-calvin-and-hobbes.jpg"}]
   [:h3 "Some secondary heading"]
   [:p "Some paragrpah. Hello. Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum."]
   [:p "Some paragrpah. Hello. Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum."]
   [:img {:src "https://geneticliteracyproject.org/wp-content/uploads/2017/06/AdobeStock_276219341.jpeg"}]
   [:p "Some paragrpah. Hello. Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum."]
   [:h3 "Some other heading goes here"]
   [:p "Some paragrpah. Hello. Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum."]
   [:ul
    [:li "This is a first point about something important."]
    [:li "This is a second point about something important. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged."]
    [:li "This is some other short point."]]
   [:p "Some paragrpah. Hello. Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum."]])

(defn component-interface [account]
  (let [post-ui (r/atom {:post "" :state nil})]
    (fn []
      (let [post (@post-ui :post)
            state (@post-ui :state)
            has-content? (> (.-length post) 0)]
        [:div#post {:class (if state (str "is-" (name state)))}
         [:p "Your public key: " (get-in @account ["keys" "public-key"])]
         [:textarea (merge 
                      {:rows (js/Math.max 5 (.-length (.split post "\n")))
                       :value post
                       :on-change #(swap! post-ui assoc :post (.. % -target -value))}
                      (if (= state :posting) {:disabled true}))]
         (when (= state :error) [:p.error "Error: " (@post-ui :error)])
         [:button (if (and has-content? (not= state :posting)) {:on-click (partial post! post-ui account post)} {:disabled true})
          [:span (if (= state :posting) "|" "Post")]]]))))

(defn component-setup-profile [account]
  
  )

(defn component-setup-keys [account]
  (fn []
    [:div.interface
     [:h3 "You don't seem to have an account yet."]
     [:p "A Bitwalden account is simply a cryptographic key pair."]
     [:button {:on-click (partial create-new-key account)} "Create a new key pair now"]]))

(defn component-container [account]
  [:div
   [:div#logo
    [:h2 "Bitwalden"]
    [:h3 "reference client"]]
   [:div#content
    (cond
      (not (@account "keys")) [component-setup-keys account]    
      ;(not (@account "profile")) [component-setup-profile account]
      :else [component-interface account])]])

;; -------------------------
;; Initialize app

; refresh the lists of nodes we know about
(go
  (when (= (count @nodes) 0)
    (reset! nodes (<! (refresh-known-nodes))))
  (print "known-nodes:" @nodes))

(defn mount-root []
  (r/render [component-container account] (.getElementById js/document "app")))

(defn init! []
  (mount-root))
