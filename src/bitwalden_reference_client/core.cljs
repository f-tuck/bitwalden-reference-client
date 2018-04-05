(ns bitwalden-reference-client.core
  (:require
    [reagent.core :as r]
    [cljs.core.async :refer [<!]]
    [bitwalden-reference-client.lib.core :as bitwalden]
    [bitwalden-reference-client.lib.rpc :refer [refresh-known-nodes]]
    [bitwalden-reference-client.lib.data :as accounts]
    [bitwalden-reference-client.lib.crypto :refer [generate-keypair-seed-b58 keypair-from-seed-b58 keypair-from-private-key-b58 public-key-b58-from-keypair private-key-b58-from-keypair]]
    [bitwalden-reference-client.lib.util :refer [random-hex now]]
    [bitwalden-reference-client.ui.processing :refer [proc+! proc-!]])
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

(defn create-new-account! [account ev]
  (let [seed (generate-keypair-seed-b58)
        keypair (keypair-from-seed-b58 seed)
        pk (public-key-b58-from-keypair keypair)
        sk (private-key-b58-from-keypair keypair)]
    (swap! account #(-> %
                        (assoc "keys" {"private-key" sk "public-key" pk})
                        (assoc "public" {"feed" (accounts/make-json-feed pk) "profile" (accounts/make-profile pk)})
                        (assoc-in ["private" "following"] ["TuckJdmdsXkjrh7rUPtPTDKp7SMhKnj918HT7zNKN2v" pk])))
    (save-account! @account)))

(defn get-keypair [account-data]
  (let [seed (get-in account-data ["keys" "seed"])
        private-key (get-in account-data ["keys" "private-key"])]
    (cond
      seed (keypair-from-seed-b58 seed)
      private-key (keypair-from-private-key-b58 private-key))))

(defn get-nodes [account-data]
  (get-in account-data ["cache" "known-good-nodes"]))

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
      (js/console.log "post! response:" node (clj->js post-response))
      (if (post-response "error")
        (swap! post-ui assoc :state :error :error (post-response "message"))
        (do
          (when (and (post-response :profile) (post-response :feed))
            (update-account! account (post-response :profile) (post-response :feed)))
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
            (go
              (print "Refreshing" public-key-base58)
              (proc+! account (str "Refreshing " public-key-base58))
              (let [node (rand-nth (get-nodes @account))
                    [profile feed] (<! (refresh-account node keypair public-key-base58))]
                (print "Response from node" node "for" public-key-base58)
                (when (and profile feed)
                  (swap! account assoc-in ["cache" "following" public-key-base58] {"profile" profile "feed" feed "updated" (now)})
                  (save-account! @account))
                (proc-! account (str "Refreshing " public-key-base58))))
            (print "Not refreshing" public-key-base58)))))))

(defn refresh-own-account! [account]
  (let [keypair (get-keypair @account)
        public-key-base58 (public-key-b58-from-keypair keypair)
        node (rand-nth (get-nodes @account))]
    (go
      (proc+! account (str "Refreshing own account"))
      (when (and node keypair)
        (print "Refreshing account feed & profile from:" node)
        (let [refresh-response (<! (bitwalden/refresh-account node keypair public-key-base58))]
          ; if response swap account
          (print "Response:" refresh-response)
          (proc-! account (str "Refreshing own account"))
          (when (not (refresh-response :error))
            (update-account! account (refresh-response :profile) (refresh-response :feed))))))))

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

(defn initialize [first-load]
  (go
    (let [account (r/atom (or (load-account) (accounts/make-empty-account)))
          keypair (get-keypair @account)]
      ; in dev mode bind account to window
      (bind-account-to-window! account)
      ; make sure we have some known nodes
      (when (= (count (get-nodes @account)) 0)
        (print "Refreshing known-nodes list.")
        (swap! account assoc-in ["cache" "known-good-nodes"] (<! (refresh-known-nodes (get-nodes @account)))))
      ; refresh nodes and account
      (print "Known-nodes:" (get-nodes @account))
      ; try to load our crypto keys
      (if (and keypair first-load)
        ; restore our account from localStorage
        (let [public-key-base58 (public-key-b58-from-keypair keypair)]
          (print "Using public key:" public-key-base58)
          ; update the user's own account
          (<! (refresh-own-account! account))
          (save-account! @account)
          (go (refresh-followers! account)))
        (print "Non-first load or no keypair."))
      account)))
