(ns  misc.rss-downloader
  (:require [clojure.string :as str]
    [clojure.data.xml :as xml]
    [clojure.java.io :as io])
  (:import java.security.MessageDigest
    java.io.FileInputStream
    java.io.File
    java.nio.ByteBuffer))


(def document (xml/parse-str (slurp "resources/rss/serial-killers.xml")))

(defn parse-item [i] 
  (let [name (->> i 
               :content
               (filter #(= (:tag %1) :title))
               first
               :content
               first
               )
        link (->> i 
               :content
               (filter #(= (:tag %1) :enclosure))
               first
               :attrs
               :url
               )] [name link]))

(defn copy-uri-to-file [uri file]
  (with-open [in (clojure.java.io/input-stream uri)
              out (clojure.java.io/output-stream file)]
    (clojure.java.io/copy in out))
  file)

(defn download [[name link]] 
  (let [file-name (-> name
                    (str/replace #"\?|\"|:|\||“|”" "")
                    (.trim))
        file-name (str "tmp/" file-name ".mp3")]
    (if (not (.exists (java.io.File. file-name)))
      (copy-uri-to-file link file-name)
      file-name)))

(defn calculate-md5 [file-path]
  (let [data (-> file-path
               File.
               .toPath
               java.nio.file.Files/readAllBytes)
        hash (.digest (MessageDigest/getInstance "MD5") data)
        checksum (BigInteger. 1 hash)]
    (with-open [wrtr (io/writer "tmp/hashes.txt" :append true)]
      (.write wrtr (str file-path " "(.toString checksum 16 ) "\n")))))

(->> document
  :content
  first
  :content
  (filter #(= :item (:tag %1)))
  (map parse-item)
  (map download)
  (map calculate-md5))


