(ns bitwalden-reference-client.ui.views
  (:require [bitwalden-reference-client.ui.processing :refer [unpack-processing component-processing-indicator]]
            [bitwalden-reference-client.lib.crypto :refer [private-key-b58-from-keypair]]
            [bitwalden-reference-client.core :refer [post! get-keypair add-public-key-to-follow! update-private-key! create-new-account! get-nodes refresh-followers!]]
            [bitwalden-reference-client.lib.data :refer [sort-posts-by-date merge-posts merge-profile-into-feed-items]]
            [bitwalden-reference-client.lib.util :refer [format-date]]
            [reagent.core :as r]))

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
         [:p "Your public key: "
          [:input {:read-only true :value (get-in @account ["keys" "public-key"])}]]
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
     (do
       ;(print "f id:" (str f))
       (let [handle (get-in f ["profile" "handle"])]
         (when (= (f "content_format") "markdown")
           (with-meta
             [:div.feed-item
              [:div.date (format-date (f "date_published"))]
              [:div.author
               (comment [:img {:src (get-in f ["profile" "avatar-url"])}])
               (when handle [:span.handle handle])
               [:span.public-key (get-in f ["profile" "pk"])]]
              [:pre (f "content_text")]]
             {:key (f "id")})))))])

(defn component-main [account]
  [:div#main
   [component-interface account]
   [component-feeds account]])

(defn component-profile-input [account display-name path]
  )

(defn component-config [account]
  (let [show-private-key-interface (r/atom false)
        public-key-to-follow (r/atom "")
        keypair (get-keypair @account)
        private-key-base58 (private-key-b58-from-keypair keypair)
        updated-private-key (r/atom private-key-base58)]
    (fn []
      [:div#config
       [:h3 "Add a follower"]
       [:p
        [:input {:placeholder "Paste a public key in base58 format to follow" :value @public-key-to-follow :on-change #(reset! public-key-to-follow (.. % -target -value))}]
        [:button {:on-click (fn [ev] (add-public-key-to-follow! account @public-key-to-follow) (reset! public-key-to-follow ""))} "Follow"]]
       [:h3 "Key management"]
       [:p
        (if @show-private-key-interface
          [:span [:input {:placeholder "Paste your private key in base58 format" :value @updated-private-key :on-change #(reset! updated-private-key (.. % -target -value))}]
           [:button {:on-click (fn [ev] (update-private-key! account @updated-private-key) (swap! show-private-key-interface not))} "Update"] 
           [:button.danger {:on-click #(swap! show-private-key-interface not)} "Cancel"]]
          [:span
           [:button.danger {:on-click #(swap! show-private-key-interface not)} "🔒 Reveal / set private key"]])]
       [:h3 "Your profile"]
       [:p
        [:input {:placeholder "Name / handle"
                 :on-change #(swap! account assoc-in ["public" "profile" "handle"] (.. % -target -value))
                 :value (get-in @account ["public" "profile" "handle"])}]]])))

(defn component-setup-keys [account]
  (fn []
    [:div.interface
     [:h3 "You don't seem to have an account yet."]
     [:p "A Bitwalden account is secured with a cryptographic key pair."]
     [:button {:on-click (partial create-new-account! account)} "Create a new key pair now"]]))

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
  (let [page (r/atom nil)]
    (fn []
      [:div
       [:div#logo
        [:h2 "Bitwalden"]
        [:h3 "reference client"]]
       [:div#actions
        (if ((unpack-processing @account) :active?)
          [component-processing-indicator @account]
          [:span#indicator {:on-click #(refresh-followers! account)} "↺"])
        [:span {:on-click #(swap! page (fn [p] (if (nil? p) "config")))} "⛭"]]
       [:div#content
        (cond
          (= (count (get-nodes @account)) 0) [component-no-nodes @account]
          (not (@account "keys")) [component-setup-keys account]
          (= @page "config") [component-config account]
          ;(not (@account "profile")) [component-setup-profile account]
          :else [component-main account])]])))

