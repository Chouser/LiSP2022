(ns us.chouser.lisp2022.status
  (:require [clojure.java.io :as io]
            [clj-http.client]
            [clojure.data.json :as json]
            [oz.core :as oz]
            [clojure.string :as str]))

(defn slack [xoxb method & [query-params]]
  (let [req {:url (str "https://slack.com/api/" (name method))
             :method :post
             :accept :json
             :headers {"Authorization" (str "Bearer " xoxb)}
             :query-params query-params}
        resp (clj-http.client/request req)
        body-data (json/read-str (:body resp) :key-fn keyword)]
    (when (-> body-data :response_metadata :next_cursor seq)
      (println "WARN: next_cursor was returned"))
    (when-not (-> body-data :ok)
      (throw (ex-info (str "Slack returned error: "
                           (pr-str (:error body-data)))
                      {:slack-data body-data})))
    body-data))

(defn all-user-profiles [xoxb]
  (lazy-seq
   (->> (:members (slack xoxb :users.list))
        (map (fn [user]
               (let [{:keys [profile]} (slack xoxb :users.profile.get
                                              {:user (-> user :id)})]
                 (apply merge
                        (dissoc user :profile)
                        (dissoc profile :fields)
                        (map (fn [[k v]]
                               {k (-> profile :fields v :value)})
                             {:next-chapter-start :Xf03EBCRLYCQ
                              :last-section-done  :Xf03DML42MGB}))))))))

(def col04 " ⡀⡄⡆⡇")
(def col08 " ⡀⣀⣄⣤⣦⣶⣷⣿")
(def col08 " ▁▂▃▄▅▆▇█")

(def pairs
  [[" ⢀⢠⢰⢸"]
   ["⡀⣀"]
   ["⡄"]
   ["⡆"]
   ["⡇"]])

(def others "
⡀  ⡁  ⡂  ⡃  ⡄  ⡅  ⡆  ⡇  ⡈  ⡉  ⡊  ⡋  ⡌  ⡍  ⡎  ⡏

⡐  ⡑  ⡒  ⡓  ⡔  ⡕  ⡖  ⡗  ⡘  ⡙  ⡚  ⡛  ⡜  ⡝  ⡞  ⡟

⡠  ⡡  ⡢  ⡣  ⡤  ⡥  ⡦  ⡧  ⡨  ⡩  ⡪  ⡫  ⡬  ⡭  ⡮  ⡯

⡰  ⡱  ⡲  ⡳  ⡴  ⡵  ⡶  ⡷  ⡸  ⡹  ⡺  ⡻  ⡼  ⡽  ⡾  ⡿

⢀  ⢁  ⢂  ⢃  ⢄  ⢅  ⢆  ⢇  ⢈  ⢉  ⢊  ⢋  ⢌  ⢍  ⢎  ⢏

⢐  ⢑  ⢒  ⢓  ⢔  ⢕  ⢖  ⢗  ⢘  ⢙  ⢚  ⢛  ⢜  ⢝  ⢞  ⢟

⢠  ⢡  ⢢  ⢣  ⢤  ⢥  ⢦  ⢧  ⢨  ⢩  ⢪  ⢫  ⢬  ⢭  ⢮  ⢯

⢰  ⢱  ⢲  ⢳  ⢴  ⢵  ⢶  ⢷  ⢸  ⢹  ⢺  ⢻  ⢼  ⢽  ⢾  ⢿

⣀  ⣁  ⣂  ⣃  ⣄  ⣅  ⣆  ⣇  ⣈  ⣉  ⣊  ⣋  ⣌  ⣍  ⣎  ⣏

⣐  ⣑  ⣒  ⣓  ⣔  ⣕  ⣖  ⣗  ⣘  ⣙  ⣚  ⣛  ⣜  ⣝  ⣞  ⣟

⣠  ⣡  ⣢  ⣣  ⣤  ⣥  ⣦  ⣧  ⣨  ⣩  ⣪  ⣫  ⣬  ⣭  ⣮  ⣯

⣰  ⣱  ⣲  ⣳  ⣴  ⣵  ⣶  ⣷  ⣸  ⣹  ⣺  ⣻  ⣼  ⣽  ⣾  ⣿
")

(defn clipstr
  "Return a string of exactly length `len` by either truncating or adding spaces
  to the end of `string`."
  [string len]
  (if (< (count string) len)
    (str string (apply str (repeat (- len (count string)) " ")))
    (subs string 0 len)))

(def yyyy-MM-dd (java.text.SimpleDateFormat. "yyyy-MM-dd"))
(def MM-dd (java.text.SimpleDateFormat. "MM-dd"))

(defn histogram [xs minrange labelfn]
  (let [chars col08
        xscale 40
        yscale 8
        maxchar (dec (count chars))
        minx (apply min xs)
        maxx (apply max xs)
        [minx maxx] (if (< minrange (- maxx minx))
                      [minx maxx]
                      (let [minx (/ (+ minx maxx (- minrange)) 2)]
                        [minx (+ minx minrange)]))
        xfactor (/ xscale (- maxx minx))
        freqs (frequencies (map #(Math/round (float (* xfactor (- % minx)))) xs))
        maxy (apply max (vals freqs))
        yfactor (/ yscale (inc maxy))
        peakcol (some (fn [[k v]] (when (= v maxy) k)) freqs)
        peakx (+ minx (/ peakcol xfactor))
        labels [[0 (str "^ " (labelfn minx))]
                [(dec peakcol) (str " ^ " (labelfn peakx))]
                [xscale (str "^ " (labelfn maxx))]]]
    (concat
     (for [row (range (/ yscale maxchar) 0 -1)]
       (apply str
              (for [x (range (inc xscale))]
                (get chars
                     (let [y (int (+ 0.9999 (* yfactor (freqs x 0))))]
                       (cond
                         (< y (* (dec row) maxchar)) 0
                         (>= y (* row maxchar)) maxchar
                         :else (rem y maxchar)))))))
     [(reduce (fn [s [offset label]]
                (str (clipstr s offset) label))
              ""
              labels)])))

(defn go1 []
  (let [{:keys [xoxb]} (read-string (slurp "secrets.edn"))]
    (def x (all-user-profiles xoxb))

    #_
    (slack xoxb :chat.postMessage
           {:channel "U03BTEC3352"
            :blocks (json/write-str [{"type" "image",
                                      "block_id" "image4",
                                      "image_url" "",
                                      "alt_text" "An incredibly cute kitten."}])})))

(defn go2 []
  (let [{:keys [xoxb]} (read-string (slurp "secrets.edn"))
        xs (concat
            (keep :next-chapter-start x)
            (repeatedly 30 #(format "2022-07-%02d" (rand-int 30))))
        lines (histogram (map #(.getTime (.parse yyyy-MM-dd %)) xs)
                         (* 40 1000 60 60 24)
                         #(.format MM-dd %))]
    (slack xoxb :chat.postMessage
           {:channel "U03BTEC3352"
            :blocks (json/write-str
                     [{:type :header
                       :text {:type :plain_text
                              :text "Progress report aggregation"}}
                      {:type :section
                       :text {:type :mrkdwn
                              :text (str/join
                                     "\n"
                                     (concat ["When should we start Chapter 2?"
                                              "```"]
                                             lines
                                             ["```"]))}}])})))

(comment

  (map #(select-keys % [:display_name :next-chapter-start :last-section-done]) (filter :next-chapter-start x))

  (let [sample (repeatedly 40 #(format "2022-05-%02d" (rand-int 10)))]
    (run! println (histogram (map #(.getTime (.parse yyyy-MM-dd %)) sample)
                             (* 40 1000 60 60 24)
                             #(.format MM-dd %))))

  :end)
