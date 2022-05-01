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
    #_(def x (all-user-profiles xoxb))

    (slack xoxb :chat.postMessage
           {:channel "U03BTEC3352"
            :blocks (json/write-str [{"type" "image",
                                      "block_id" "image4",
                                      "image_url" "data:image/png;base64,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",
                                      "alt_text" "An incredibly cute kitten."}])})))



(comment
  (oz/start-plot-server!)
  (oz/v!
   {:data {:values (map #(assoc % :y (rand-int 10))
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
