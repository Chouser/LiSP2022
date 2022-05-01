(ns us.chouser.lisp2022.status
  (:require [clojure.java.io :as io]
            [clj-http.lite.client :as http]
            [clojure.data.json :as json]
            [oz.core :as oz]))

(defn slack [xoxb method & [query-params]]
  (let [req {:url (str "https://slack.com/api/" (name method))
             :method :post
             :accept :json
             :headers {"Authorization" (str "Bearer " xoxb)}
             :query-params query-params}
        resp (http/request req)
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

(defn go []
  (let [{:keys [xoxb]} (read-string (slurp "secrets.edn"))]
    (def x (all-user-profiles xoxb))

    #_
    (slack xoxb :chat.postMessage
           {:channel "U03BTEC3352"
            :blocks (json/write-str [{"type" "image",
                                      "block_id" "image4",
                                      "image_url" "",
                                      "alt_text" "An incredibly cute kitten."}])})))

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

(def sample
  (map (fn [_]
         (let [x (- (* 2 (rand)) 1)]
           (int (* x x x 30))))
       (range 40)))

(defn clipstr
  "Return a string of exactly length `len` by either truncating or adding spaces
  to the end of `string`."
  [string len]
  (if (< (count string) len)
    (str string (apply str (repeat (- len (count string)) " ")))
    (subs string 0 len)))

(defn histogram [xs]
  (let [chars col08
        xscale 40
        yscale 8
        maxchar (dec (count chars))
        minx (apply min xs)
        maxx (apply max xs)
        xfactor (/ xscale (inc (- maxx minx)))
        freqs (frequencies (map #(Math/round (float (* xfactor (- % minx)))) xs))
        maxy (apply max (vals freqs))
        yfactor (/ yscale (inc maxy))
        peakx (some (fn [[k v]] (when (= v maxy) k)) freqs)
        labels [[0 (str "^ " minx)]
                [peakx (str "^ " (float (+ minx (/ peakx xfactor))))]
                [(dec xscale) (str "^ " maxx)]]]
    (concat
     (for [row (range (/ yscale maxchar) 0 -1)]
       (apply str
              (for [x (range xscale)]
                (get chars
                     (let [y (int (+ 0.9 (* yfactor (freqs x 0))))]
                       (cond
                         (< y (* (dec row) maxchar)) 0
                         (>= y (* row maxchar)) maxchar
                         :else (rem y maxchar)))))))
     [(reduce (fn [s [offset label]]
                (str (clipstr s offset) label))
              ""
              labels)])))

(comment
  (map #(select-keys % [:display_name :next-chapter-start :last-section-done]) (filter :next-chapter-start x))

  (oz/start-plot-server!)
  (oz/v!
   {:data {:values (map #(-> %
                             (select-keys [:next-chapter-start])
                             (assoc :y (rand-int 10)))
                    (concat
                     x
                     (for [x (range 100)]
                       {:next-chapter-start (format "2022-%02d-%02d"
                                                    (+ 9 (rand-int 3))
                                                    (inc (rand-int 29)))})))}
    :mark "circle"
    :encoding {:x {:field :next-chapter-start :type :temporal
                   :axis {:grid false, :title "When to start chapter 4"}}
               :y {:field :y :type :quantitative
                   :axis nil}}
    :config {:style {:cell {:stroke :transparent}}}
    :width 500
    :height 20})


  :end)
