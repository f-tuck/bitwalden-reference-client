(ns bitwalden-reference-client.core
  (:require
    [reagent.core :as r]
    [cljs.core.async :refer [<!]]
    [bitwalden-client-lib.core :as bitwalden]
    [bitwalden-client-lib.rpc :refer [refresh-known-nodes]]
    [bitwalden-client-lib.data :as accounts]
    [bitwalden-client-lib.crypto :refer [generate-keypair-seed-b58 keypair-from-seed-b58 keypair-from-private-key-b58 public-key-b58-from-keypair private-key-b58-from-keypair]]
    [bitwalden-client-lib.util :refer [random-hex]])
  (:require-macros
    [cljs.core.async.macros :refer [go]]))

(def account-key "bitwalden-account")

(defonce account-debug (atom nil))

(defn load-account []
  (let [raw-data (js/JSON.parse (.getItem (aget js/window "localStorage") account-key))]
    (js/console.log "Account:" raw-data)
    (js->clj raw-data)))

(defn save-account! [account-data]
  (.setItem (aget js/window "localStorage") account-key (js/JSON.stringify (clj->js (dissoc account-data :state))))
  account-data)

(defn update-account! [account profile feed]
  (swap! account #(-> %
                      (assoc-in ["public" "profile"] profile)
                      (assoc-in ["public" "feed"] feed)
                      (assoc-in ["cache" "following" (profile "pk") "profile"] profile)
                      (assoc-in ["cache" "following" (profile "pk") "feed"] feed))))

(defn bind-account-to-window! [account]
  (reset! account-debug account)
  (aset js/window "account" #(clj->js @account)))

(defn create-new-key! [account ev]
  (let [seed (generate-keypair-seed-b58)
        keypair (keypair-from-seed-b58 seed)
        pk (public-key-b58-from-keypair keypair)]
    (swap! account #(-> %
                        (assoc "keys" {"seed" seed "public-key" pk})
                        (assoc-in ["public" "feed"] (accounts/make-json-feed pk))
                        (assoc-in ["public" "profile"] (accounts/make-profile pk))
                        (assoc-in ["private" "following"] ["TuckJdmdsXkjrh7rUPtPTDKp7SMhKnj918HT7zNKN2v" pk])))
    (save-account! @account)))

(defn get-keypair [account-data]
  (let [seed (get-in account-data ["keys" "seed"])
        private-key (get-in account-data ["keys" "private-key"])]
    (cond
      seed (keypair-from-seed-b58 seed)
      private-key (keypair-from-private-key-b58 private-key))))

(defn add-processing-item! [account id]
  (swap! account update-in [:state :processing] #(into #{id} %)))

(defn remove-processing-item! [account id]
  (swap! account update-in [:state :processing] clojure.set/difference #{id}))

(defn get-nodes [account-data]
  (get-in account-data ["cache" "known-good-nodes"]))

(defn now []
  (.getTime (js/Date.)))

(defn post! [post-ui account content ev]
  (print "post!")
  (swap! post-ui assoc :state :posting)
  (go
    (let [nodes (get-in @account ["cache" "known-good-nodes"])
          profile (get-in @account ["public" "profile"])
          feed (get-in @account ["public" "feed"])
          keypair (get-keypair @account)
          post-struct (accounts/make-post (random-hex 32) content)
          node (rand-nth nodes)
          post-response (<! (bitwalden/add-post! node keypair post-struct profile feed))]
      (js/console.log "post! response:" (clj->js post-response))
      (if (post-response "error")
        (swap! post-ui assoc :state :error :error (post-response "message"))
        (do
          (update-account! account (post-response :profile) (post-response :feed))
          (swap! post-ui assoc :state nil :post "" :error nil)
          (save-account! @account)))))
  (.preventDefault ev)
  (.blur (.. ev -target)))

(defn refresh-account [node keypair public-key-base58]
  (go
    (let [refresh-response (<! (bitwalden/refresh-account node keypair public-key-base58))]
      ; if response swap account
      (print "refresh-response:" refresh-response)
      (when (not (refresh-response :error))
        [(refresh-response :profile) (refresh-response :feed)]))))

(defn refresh-followers! [account]
  (go
    (let [keypair (get-keypair @account)
          following (get-in @account ["private" "following"])
          cached (get-in @account ["cache" "following"])]
      (print "Following:" following)
      (when (and (get-nodes @account) keypair)
        (print "Refreshing follower accounts.")
        ; TODO: run these in parallel batches of 10
        (doseq [public-key-base58 following]
          (if (< (get-in cached [public-key-base58 "updated"]) (- (now) (* 5 60 1000)))
            (do
              (print "Refreshing" public-key-base58)
              (add-processing-item! account (str "Refreshing " public-key-base58))
              (let [[profile feed] (<! (refresh-account (rand-nth (get-nodes @account)) keypair public-key-base58))]
                (if (and profile feed)
                  (swap! account assoc-in ["cache" "following" public-key-base58] {"profile" profile "feed" feed "updated" (now)}))
                (remove-processing-item! account (str "Refreshing " public-key-base58))))
            (print "Not refreshing" public-key-base58)))))))

(defn merge-profile-into-feed-items [[public-key-base58 data]]
  (map #(assoc % "profile" (data "profile"))
       (get-in data ["feed" "items"])))

(defn merge-posts [account-data]
  (apply concat
         (map merge-profile-into-feed-items
              (get-in account-data ["cache" "following"]))))

(defn sort-posts-by-date [merged-feed-items]
  (reverse (sort-by #(get % "date_published") merged-feed-items)))

(defn add-public-key-to-follow! [account public-key-base58]
  (swap! account update-in ["private" "following"]
         (fn [following]
           (vec (conj (set following) public-key-base58))))
  (save-account! @account)
  (refresh-followers! account))

(defn update-private-key! [account new-private-key-base58]
  (if (js/confirm "This will destroy the currently loaded account and replace it with the new one. Are you sure?")
    (go
      ; TODO: error checks
      (let [keypair (keypair-from-private-key-b58 new-private-key-base58)
            public-key-base58 (public-key-b58-from-keypair keypair)
            nodes (get-nodes @account)]
        (reset! account {"keys" {"private-key" new-private-key-base58 "public-key" public-key-base58}
                         "cache" {"known-good-nodes" nodes}})
        (save-account! @account)

        (<! (refresh-own-account! account))
        (save-account! @account)
        (refresh-followers! account)))))

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
          (if (= state :posting)
            [:span.spinner " "]
            [:span "Post"])]]))))

(defn component-feeds [account]
  [:div
   (for [f (sort-posts-by-date (merge-posts @account))]
     (let [nom-de-plume (get-in f ["profile" "name"])]
       (when (= (f "content_format") "markdown")
         [:div#feed-item {:key (f "id")}
          [:div.author
           (comment [:img {:src (get-in f ["profile" "avatar-url"])}])
           (when nom-de-plume [:span.name nom-de-plume])
           [:span.public-key (get-in f ["profile" "pk"])]]
          [:div.date (f "date_published")]
          [:pre (f "content_text")]])))])

(defn component-main [account]
  [:div#hello
   [component-interface account]
   [component-feeds account]])

(defn component-setup-profile [account]
  
  )

(defn component-setup-keys [account]
  (fn []
    [:div.interface
     [:h3 "You don't seem to have an account yet."]
     [:p "A Bitwalden account is simply a cryptographic key pair."]
     [:button {:on-click (partial create-new-key! account)} "Create a new key pair now"]]))

(defn component-no-nodes [account-data]
  [:div
   [:p.error "Can't reach any Bitwalden nodes."]
   (if (= (count (get-nodes account-data)) 0)
     [:div "No nodes known."]
     [:div
      [:p "Tried:"]
      [:ul
       (doall (for [n (get-nodes account-data)]
                [:li n]))]])])

(defn component-container [account]
  (fn []
    (let [processing (> (count (get-in @account [:state :processing])) 0)]
      [:div
       [:div#logo
        [:h2 "Bitwalden"]
        [:h3 "reference client"]]
       (if processing
         [:div#indicator.spinner]
         [:div#indicator {:on-click #(refresh-followers! account)} "↺"])
       [:div#content
        (cond
          (= (count (get-nodes @account)) 0) [component-no-nodes @account]
          (not (@account "keys")) [component-setup-keys account]
          ;(not (@account "profile")) [component-setup-profile account]
          :else [component-main account])]])))

;; -------------------------
;; Initialize app

(defn mount-root [& args]
  (print "mount-root" args)
  (if (nil? args)
    ; restore our account from localStorage
    (let [account (r/atom (or (load-account) (accounts/make-empty-account)))
          keypair (keypair-from-seed-b58 (get-in @account ["keys" "seed"]))
          public-key-base58 (get-in @account ["keys" "public-key"])]
      ; in dev mode bind account to window
      (bind-account-to-window! account)
      (print "Loaded data:" public-key-base58)
      ; refresh nodes and account
      (go
        (when (= (count (get-nodes @account)) 0)
          (print "Refreshing known-nodes list.")
          (swap! account assoc-in ["cache" "known-good-nodes"] (<! (refresh-known-nodes (get-nodes @account)))))
        (print "Known-nodes:" (get-nodes @account))
        ; update the user's own account
        (when (and (get-nodes @account) keypair)
          (print "Refreshing account feed & profile.")
          (let [node (rand-nth (get-nodes @account))
                refresh-response (<! (bitwalden/refresh-account node keypair public-key-base58))]
            ; if response swap account
            (print "Response:" refresh-response)
            (when (not (refresh-response :error))
              (update-account! account (refresh-response :profile) (refresh-response :feed)))))
        (save-account! @account)
        (refresh-followers! account)
        (print "profile:" (@account "public"))
        (r/render [component-container account] (.getElementById js/document "app"))))
    (print "Non-first load.")))

(defn init! []
  (mount-root))
