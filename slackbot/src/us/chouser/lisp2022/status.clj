(ns us.chouser.lisp2022.status
  (:require [clojure.java.io :as io]
            [clj-http.lite.client]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.edn :as edn]))

(def channel-progress "C03DKM2M7SA")
(def user-chouser "U03BTEC3352")
(def user-bot "U03DF1E6KDL")

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
    (subs string 0 (max 0 len))))

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
    1.4.0 1.4.1 1.4.2 1.4.3 1.4.4 1.4.5 1.4.6 1.4
    1.5
    1.6.0 1.6.1 1.6.2 1.6
    1.7
    1.8
    1.9
    1.10 1.10.11
    2.0
    2.1
    2.2.0 2.2.1 2.2.2 2.2.3 2.2.4 2.2
    2.3
    2.4
    2.5.0 2.5.1 2.5.2 2.5.3 2.5.4 2.5
    2.6.0 2.6.1 2.6.2 2.6.3 2.6.4 2.6.5 2.6.6 2.6
    2.7
    2.8
    3.1
    3.1.0 3.1.1 3.1.2 3.1.3 3.1.4.3.1.5 3.1.6
    3.2.0 3.2.1 3.2.2 3.2.3 3.2.4 3.2.5 3.2.6 3.2.7
    3.3
    3.4.0 3.4.1 3.4.2 3.4.3 3.4.4
    3.5
    3.6.0 3.6.1 3.6 2
    3.7
    3.8
    3.9
    4.1 4.1.1 4.1.2 4.1.3
    4.2 4.2.1 4.2.2
    4.3 4.3.1 4.3.2 4.3.3 4.3.4 4.3.5 4.3.6 4.3.7 4.3.8 4.3.9 4.3.10 4.3.11
        4.3.12 4.3.13 4.3.14
    4.4 4.5 4.6 4.7
    5.1
    5.2 5.2.1 5.2.2 5.2.3 5.2.4 5.2.5 5.2.6 5.2.7 5.2.8
    5.3 5.4 5.5 5.6
    5.7 5.7.1 5.7.2 5.7.3
    5.8
    5.9 5.9.1 5.9.2
    5.10 5.11")))

(def sections-index (zipmap sections (range)))
(def index-sections (zipmap (range) sections))

(defn parse-progress [text]
  (try
    (when-let [[_ m] (re-find #"(\{.*)" text)]
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
                (let [my-rs (->> (:reactions message)
                                 (filter (fn [r] (some (partial = user-bot) (:users r))))
                                 (map :name)
                                 set)
                      new-r (if (:error p) "x" "white_check_mark")
                      msg-id {:channel channel-progress
                              :timestamp (:ts message)}]
                  (when (:error p)
                    (prn (:error p) (:text message)))
                  (when-not (my-rs new-r)
                    (slack xoxb :reactions.add (assoc msg-id :name new-r)))
                  (run!
                   #(slack xoxb :reactions.remove (assoc msg-id :name %))
                   (disj my-rs new-r))))
              (if (or (not p) (:error p))
                progress
                (update progress (:user message)
                        (fn [oldp]
                          (if (and (:ts oldp) (<= mts (:ts oldp)))
                            oldp
                            (assoc p :ts mts)))))))
          progress
          (:messages (slack xoxb :conversations.history
                            {:channel channel-progress
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

(defn merge-latest-progress [state]
  (merge-with
   (fn [a b]
     (if (< (:ts a) (:ts b))
       (merge a b)
       (merge b a)))
   (:progress state)
   (update-vals (:profiles state)
                (fn [profile]
                  (cond-> {:real_name (:real_name profile)
                           :ts (:updated profile)}
                    (:last-section-done profile) (assoc :last-section-done (:last-section-done profile))
                    (:next-chapter-start profile) (assoc :next-chapter-start
                                                         (.parse yyyy-MM-dd (:next-chapter-start profile))))))))

(defn date-to-ts ^long [^java.util.Date d]
  (quot (.getTime d) 1000))

(defn ^java.util.Date ts-to-date [^long ts]
  (java.util.Date. (* 1000 (long ts))))

(defn remove-status-before [end-prev-chapter-date-str]
  (let [end-prev-chapter-ts (date-to-ts (.parse yyyy-MM-dd end-prev-chapter-date-str))]
    (filter #(and (:ts %)
                  (< end-prev-chapter-ts (:ts %))
                  (or (nil? (:next-chapter-start %))
                      (< end-prev-chapter-ts (date-to-ts (:next-chapter-start %))))))))

(defn go2 []
  (let [{:keys [xoxb]} (read-string (slurp "secrets.edn"))
        x (into []
                (remove-status-before "2022-05-24")
                (vals (merge-latest-progress @*state)))
        done-data (keep :last-section-done x)
        done-lines (histogram {:xs (map #(get sections-index % 0) done-data)
                               :xscale 60
                               :minrange 5
                               :labelfn #(index-sections (int %))})
        next-data (concat
                   (keep :next-chapter-start x)
                   #_(repeatedly 30 #(format "2022-07-%02d" (rand-int 30))))
        next-lines (histogram {:xs (map #(.getTime %) next-data)
                               :xscale 60
                               :minrange (* 60 1000 60 60 24)
                               :labelfn #(.format MM-dd %)})]
    (slack xoxb :chat.postMessage
           {:channel channel-progress #_user-chouser
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
                                             #_#_
                                             ["```"
                                              "When should we start the next chapter?"
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
