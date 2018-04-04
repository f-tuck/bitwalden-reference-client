(ns bitwalden-reference-client.lib.crypto
  (:require [alphabase.base58 :as b58]
            [cljsjs.nacl-fast :as nacl]
            [bitwalden-reference-client.lib.util :refer [debug string-to-uint8array uint8array-to-string join-uint8arrays bencode-parameters-uint8array hexenate unhexenate]]))

; --- key management --- ;

(defn keys-from-secret [secret]
  (.fromSecretKey nacl.sign.keyPair (js/Uint8Array.from secret)))

(defn keys-from-seed [seed]
  (.fromSeed nacl.sign.keyPair (nacl.hash (string-to-uint8array seed))))

(defn pk-from-secret [secret]
  (js/Array.from (.-publicKey (keys-from-secret secret))))

(defn keypair-human-readable [k]
  {:publicKey (b58/encode (.-publicKey k))
   :secretKey (hexenate (.-secretKey k))})

(defn public-key-b58-from-keypair [keypair]
  (b58/encode (.-publicKey keypair)))

(defn private-key-b58-from-keypair [keypair]
  (b58/encode (.-secretKey keypair)))

(defn keypair-from-seed-phrase [seed-phrase]
  "Deterministic keypair from the first half of the hash of a phrase."
  (-> seed-phrase
      (string-to-uint8array)
      (nacl.hash)
      (.slice 0 32)
      (nacl.sign.keyPair.fromSeed)))

(defn keypair-from-seed-b58 [seed-b58]
  "Deterministic keypair from a b58 encoded 32 byte seed."
  (-> seed-b58
      (b58/decode)
      (.slice 0 32)
      (nacl.sign.keyPair.fromSeed)))

(defn keypair-from-private-key-b58 [private-key-b58]
  (-> private-key-b58
      (b58/decode)
      (.slice 0 64)
      (nacl.sign.keyPair.fromSecretKey)))

(defn generate-keypair-seed-b58 []
  (b58/encode (nacl.randomBytes 32)))

(defn box-key-from-seed [seed salt]
  (-> seed
      (join-uint8arrays (string-to-uint8array salt))
      (nacl.hash)
      (.slice 0 32)))

; derive from 128 bit seed so that:
; * easier for user to pass around
; * keys deterministically derivable

; --- crypto helpers --- ;

(defn sign-datastructure [keypair params]
  (debug "params" keypair params)
  (let [params-encoded (bencode-parameters-uint8array params)
        signature-hex (hexenate (nacl.sign.detached params-encoded (.-secretKey keypair)))]
    (debug "params-encoded" params-encoded)
    signature-hex))

(defn with-signature [keypair params]
  (assoc params "s" (sign-datastructure keypair params)))

(defn verify-signed-datastructure [public-key-base58 params]
  (let [sig (unhexenate (get params "s"))
        params-encoded (bencode-parameters-uint8array (dissoc params "s"))
        pk (b58/decode public-key-base58)]
    (nacl.sign.detached.verify params-encoded sig pk)))

(defn secret-box-encrypt [salt seed plaintext]
  (let [k (box-key-from-seed seed salt)
        n (nacl.randomBytes 24)]
    (join-uint8arrays n (nacl.secretbox (string-to-uint8array plaintext)))))

(defn secret-box-decrypt [salt seed cyphertext]
  (let [k (box-key-from-seed seed salt)
        n (.slice cyphertext 0 24)
        box (.slice cyphertext 24)]
    (uint8array-to-string (nacl.secretbox.open box n k))))
