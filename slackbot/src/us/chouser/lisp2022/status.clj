(ns us.chouser.lisp2022.status
  (:require [clojure.java.io :as io]
            [clj-http.lite.client]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.edn :as edn]))

(defn slack [xoxb method & [query-params]]
  (let [req {:url (str "https://slack.com/api/" (name method))
             :method :post
             :accept :json
             :headers {"Authorization" (str "Bearer " xoxb)}
             :query-params query-params}
        resp (clj-http.lite.client/request req)
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

(defn update-user-profiles [xoxb old-profiles]
  (update-vals (into {}
                 (map (juxt :id identity))
                 (:members (slack xoxb :users.list)))
               (fn [{:keys [id] :as user}]
                 (let [old-user (get old-profiles id)]
                   (if (= (:updated user) (:updated old-user))
                     old-user
                     (let [{:keys [profile]} (slack xoxb :users.profile.get
                                                    {:user id})]
                       (apply merge
                              (dissoc user :profile)
                              (dissoc profile :fields)
                              (map (fn [[k v]]
                                     {k (-> profile :fields v :value)})
                                   {:next-chapter-start :Xf03EBCRLYCQ
                                    :last-section-done  :Xf03DML42MGB}))))))))

(def col08 " ▁▂▃▄▅▆▇█")

(defn clipstr
  "Return a string of exactly length `len` by either truncating or adding spaces
  to the end of `string`."
  [string len]
  (if (< (count string) len)
    (str string (apply str (repeat (- len (count string)) " ")))
    (subs string 0 len)))

(def yyyy-MM-dd (java.text.SimpleDateFormat. "yyyy-MM-dd"))
(def MM-dd (java.text.SimpleDateFormat. "MM-dd"))

(defn histogram [{:keys [xs minrange chars xscale yscale labelfn]
                  :or {minrange 1
                       chars col08
                       xscale 40
                       yscale 8
                       labelfn identity}}]
  (let [maxchar (dec (count chars))
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

(def sections (next (.split #"\s+" "
    1.1
    1.2
    1.3
    1.4.1 1.4.2 1.4.3 1.4.4 1.4.5 1.4.6
    1.5
    1.6.1 1.6.2
    1.7
    1.8
    1.9
    1.10
    2.1
    2.2.1 2.2.2 2.2.3 2.2.4
    2.3
    2.4
    2.5.1 2.5.2 2.5.3 2.5.4
    2.6.1 2.6.2 2.6.3 2.6.4 2.6.5 2.6.6
    2.7
    2.8")))

(def sections-index (zipmap sections (range)))
(def index-sections (zipmap (range) sections))

(defn parse-progress [text]
  (try
    (when-let [[_ m] (re-find #"(\{.*})" text)]
      (-> (edn/read-string m)
          (as-> progress
            (if-let [s (:next-chapter-date progress)]
              (assoc progress :next-chapter-start
                     (or (try (doto (.parse MM-dd s)
                                (.setYear (.getYear (java.util.Date.))))
                              (catch Exception ex))
                         (.parse yyyy-MM-dd s)))
              progress)
            (if-let [s (:last-section-done progress)]
              (if-not (sections-index s)
                (throw (ex-info (str "Unknown section: " (pr-str s)) {:s s}))
                progress)
              progress)
            (if (or (not (map? progress)) (empty? progress))
              (throw (ex-info (str "Expected map keys `:next-chapter-date` "
                                   "and/or `:last-section-done1")
                              {}))
              progress))))
    (catch Exception ex
      {:error (.getMessage ex)})))

(defn update-progress [xoxb progress]
  (reduce (fn [progress message]
            (let [p (parse-progress (:text message))
                  mts (BigDecimal. (:ts message))]
              (when p
                (try
                  (slack xoxb :reactions.add {:channel "C03DKM2M7SA"
                                              :timestamp (:ts message)
                                              :name (if (:error p)
                                                      "no"
                                                      "yes")})
                  (catch Exception ex
                    (if (-> ex ex-data :slack-data :error (= "already_reacted"))
                      :ignore
                      (throw ex)))))
              (if (or (not p) (:error p))
                progress
                (update progress (:user message)
                        (fn [oldp]
                          (if (and (:ts oldp) (<= mts (:ts oldp)))
                            oldp
                            (assoc p :ts mts)))))))
          progress
          (:messages (slack xoxb :conversations.history
                            {:channel "C03DKM2M7SA"
                             :latest ""}))))

(defonce *state (atom (try (read-string (slurp "state.edn"))
                           (catch Exception ex nil))))

(defn go []
  (let [{:keys [xoxb]} (read-string (slurp "secrets.edn"))]
    (swap! *state
           (fn [state]
             (-> state
                 (update :profiles #(update-user-profiles xoxb %))
                 (update :progress #(update-progress xoxb %)))))
    (spit "state.edn" (prn-str @*state))))

(defn go1 []
  (let [{:keys [xoxb]} (read-string (slurp "secrets.edn"))]
    (def x (all-user-profiles xoxb))))

(defn go2 []
  (let [{:keys [xoxb]} (read-string (slurp "secrets.edn"))
        done-data (keep :last-section-done x)
        done-lines (histogram {:xs (map #(get sections-index % 0) done-data)
                               :xscale 60
                               :minrange 5
                               :labelfn #(index-sections (int %))})
        next-data (concat
                   (keep :next-chapter-start x)
                   #_(repeatedly 30 #(format "2022-07-%02d" (rand-int 30))))
        next-lines (histogram {:xs (map #(.getTime (.parse yyyy-MM-dd %)) next-data)
                               :xscale 60
                               :minrange (* 60 1000 60 60 24)
                               :labelfn #(.format MM-dd %)})]
    (slack xoxb :chat.postMessage
           {:channel "U03BTEC3352"
            :blocks (json/write-str
                     [{:type :header
                       :text {:type :plain_text
                              :text "Aggregated progress report"}}
                      {:type :section
                       :text {:type :mrkdwn
                              :text (str/join
                                     "\n"
                                     (concat ["What's the last section completed?"
                                              "```"]
                                             done-lines
                                             ["```"
                                              "When should we start Chapter 2?"
                                              "```"]
                                             next-lines
                                             ["```"]))}}])})))

(comment

  (map #(select-keys % [:display_name :next-chapter-start :last-section-done]) (filter :next-chapter-start (vals profs)))

  (let [sample (repeatedly 40 #(format "2022-05-%02d" (rand-int 10)))]
    (run! println (histogram (map #(.getTime (.parse yyyy-MM-dd %)) sample)
                             (* 40 1000 60 60 24)
                             #(.format MM-dd %))))

  :end)
