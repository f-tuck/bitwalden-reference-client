(ns bitwalden-reference-client.lib.crypto
  (:require [alphabase.base58 :as b58]
            [cljsjs.nacl-fast :as nacl]
            ["bencode-js/lib/index" :as bencode]
            [bitwalden-reference-client.lib.util :refer [string-to-uint8array hexenate]]))

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

; --- crypto helpers --- ;

(defn dht-compute-sig [keypair params]
  (let [params-encoded (string-to-uint8array (bencode/encode (clj->js params)))
        sig-unit (.slice params-encoded 1 (- (.-length params-encoded) 1))]
    (print "sig-unit" sig-unit)
    (hexenate (nacl.sign.detached sig-unit (.-secretKey keypair)))))

(defn with-signature [keypair params]
  (let [params-encoded (string-to-uint8array (bencode/encode (clj->js params)))
        signature (hexenate (nacl.sign.detached params-encoded (.-secretKey keypair)))]
    (assoc params :s signature)))

