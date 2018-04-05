(ns bitwalden-reference-client.lib.data)

(defn make-profile [public-key-base58]
  {"pk" public-key-base58})

(defn make-json-feed [public-key-base58]
  {"version" "https://jsonfeed.org/version/1"
   "title" (str public-key-base58 "'s feed")
   "bitwalden" {"public-key-base58" public-key-base58}
   "items" []})

(defn make-post [id content & [date-published content-format]]
  {"id" id
   "content_text" content
   "content_format" (or content-format "markdown")
   "date_published" (or date-published (.toISOString (js/Date.)))})

(defn make-empty-account []
  {"private" {"following" []}
   "public" {"feed" nil "profile" nil}
   "cache" {"known-good-nodes" [] "feeds" []}
   "keys" nil})

; *** utilities for manipulating data *** ;

(defn merge-profile-into-feed-items [[public-key-base58 data]]
  (map #(assoc % "profile" (data "profile"))
       (set (get-in data ["feed" "items"]))))

(defn merge-posts [account-data]
  (vec
    (set
      (apply concat
             (map merge-profile-into-feed-items
                  (get-in account-data ["cache" "following"]))))))

(defn sort-posts-by-date [merged-feed-items]
  (reverse (sort-by #(get % "date_published") merged-feed-items)))

