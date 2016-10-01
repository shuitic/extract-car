(ns extract-car.core
  (:gen-class)
  (:use net.cgrand.enlive-html)
  (:import java.net.URL)
  (:import java.io.InputStreamReader)
  (:import java.io.BufferedReader)
  (:import java.io.StringReader)
  (require [clojure.java.io :as io]))

(defn extract-sjis-html [url]
  "指定したURLのSJISのページをJVMで扱えるUTF-8の文字列として取得する"
  (with-open 
    [stream (io/input-stream url)
     isreader (new InputStreamReader stream "Shift_JIS")
     reader (new BufferedReader isreader)]
      (loop [reader reader result ""]+
        (let [line (. reader readLine)]
           (if (= line nil)
             result
             (do
               (recur reader (str result line))))))))

(defn extract-kakaku-html-resource [path]
  "セレクターとパスを指定してカカクコムのページのhtml-resourceを取得する"
  (let [url (str "http://kakaku.com" path)]
    (-> url
      extract-sjis-html
      StringReader.
      html-resource)))

(defn extract-kakaku-html-dom [path selector]
  "セレクターとパスを指定してカカクコムのページのDOMを取得する"
  (select (extract-kakaku-html-resource path) selector))

(defn extract-maker-links []
  "メーカーのリンクを取得する"
  (reduce concat (map
    #(extract-kakaku-html-dom "/kuruma/" [(keyword (str "div.maker" %)) :li :a]) 
    ["JP" "IP"])))

(defn extract-car-links [maker-links]
  "メーカーリンクのリストを指定して全ての車のリンクを取得する"
  (reduce concat (map 
    #(extract-kakaku-html-dom (str (:href (:attrs %))) [:div.carList :p.fontB :a])
    maker-links)))

(defn extract-td [trs name]
  "スペックテーブルの行から指定された項目のTDタグを取得する"
  (comment println name)
  (let [td (loop [trs trs]
    (if trs
      (let [[first-tr & rest-trs] trs
            [first-td second-td third-td] (:content first-tr)]
        (if (= (first (:content first-td)) name) second-td
          (if (= (first (:content second-td)) name) third-td
              (recur rest-trs))))))]
    (if td (:content td))))

(defn strip-number [num-str]
  "文字列から数値だけを抜き出す"
  (if num-str
    (apply str (rest (re-find #"(\d+),{0,1}(\d*)" num-str)))
    ""))

(defn extract-number [trs name]
  "車詳細ページの指定された項目から数値を抜き出す"
  (let [td (extract-td trs name)]
    (strip-number (first td))))

(defn extract-str [trs name]
  "車詳細ページの指定された項目から文字列を抜き出す"
  (let [td (extract-td trs name)]
    (first td)))

(defn price [trs]
  "車詳細ページから価格を抜き出す"
  (let [td (extract-td trs "新車価格")
        a (first td)
        span (first (:content a))
        price-str (first (:content span))]
    (if (= (:tag a) :span)
      (strip-number span)
      (strip-number price-str))))

(defn extract-car-info-trs [path]
  "デバッグ用のメソッド"
  (let 
    [specsheet (extract-kakaku-html-resource (str "/kuruma/ifrmspecsheet.asp?PrdKey=" (re-find #"[K]{0,1}[0-9]+" path)))]
    (select specsheet [:table#specTbl :tr]))
)

(defn extract-car-info [path]
  "指定した車のページから必要な情報を取得する"
  (comment println path)
  (let 
    [page (extract-kakaku-html-resource path)
     specsheet (extract-kakaku-html-resource (str "/kuruma/ifrmspecsheet.asp?PrdKey=" (re-find #"[K]{0,1}[0-9]+" path)))
     trs (select specsheet [:table#specTbl :tr])]
    [
      (first (select page [:li.makerLabel :a text-node]))
      (first (select page [:div#titleBox :h2 text-node]))
      (extract-str trs "動力分類")
      (extract-number trs "定員")
      (extract-number trs "車両重量")
      (extract-number trs "排気量")
      (extract-number trs "全長")
      (extract-number trs "全幅")
      (extract-number trs "全高")
      path
      (price trs)
    ]))

(defn write-car-info [row]
  (locking write-car-info
    (println
          (apply str (interpose "," row)))))

(def file-path "car-info-kakaku.csv")
(comment defn write-car-info [row]
  "車の情報を書き出すマルチスレッド対応"
  (locking write-car-info
    (with-open [w (clojure.java.io/writer  file-path :append true)]
      (.write w
        (apply str (conj (interpose "," row) "\n"))))))

(defn extract-car-info-async [path]
  "車情報を取得して書き出すマルチスレッド対応"
  (Thread/sleep 500)
  (future
    (let [car-info (extract-car-info path)]
      (write-car-info car-info)
      true)))

(def header ["メーカー" "車名" "動力分類" "定員" "重量" "排気量" "全長" "全幅" "全高" "パス" "価格"])
(defn -main
  "メイン関数"
  [& args]
  (write-car-info header)
  (apply #'and (map #(deref %)
       (map #(extract-car-info-async (:href (:attrs %)))
            (-> (extract-maker-links)
                extract-car-links))))
  true)
