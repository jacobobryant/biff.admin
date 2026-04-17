(ns com.biffweb.admin
  "Admin dashboard library for Biff web applications.

   Provides:
   - Performance metrics via tufte profiling
   - Business metrics (DAU, WAU, revenue, signups)
   - Paginated user list
   - Middleware for HTTP handler profiling
   - Helper for wrapping biff.graph resolvers with profiling"
  (:require [com.biffweb.admin.ui :as ui]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [taoensso.tufte :as tufte]
            [tick.core :as t]))

;; ============================================================
;; Profiling helpers
;; ============================================================

(defn- get-route-id
  "Default route ID extraction. Returns e.g. \"POST /stuff/:id\"."
  [ctx]
  (let [method (some-> (:request-method ctx) name str/upper-case)
        match (:reitit.core/match ctx)
        route-name (some-> match :data :name)
        route-path (some-> match :template)]
    (when (or route-name route-path)
      (str method " " (or route-name route-path)))))

(defn wrap-profiling
  "Ring middleware that wraps the handler in a tufte/p call for profiling.
   Merges pstats into the :biff.admin/pstats atom after each request."
  [handler]
  (fn [{:biff.admin/keys [pstats get-route-id*] :as ctx}]
    (let [route-id ((or get-route-id* get-route-id) ctx)]
      (if (and pstats route-id)
        (let [[result pstats-data]
              (tufte/profiled {} (tufte/p (keyword route-id) (handler ctx)))]
          (when pstats-data
            (swap! pstats (fn [existing]
                            (if existing
                              (tufte/merge-pstats existing pstats-data)
                              pstats-data))))
          result)
        (handler ctx)))))

(defn wrap-resolver-profiling
  "Wraps all resolvers in a biff.graph index with tufte profiling.
   Each resolver's :resolve function gets wrapped in a tufte/p call
   using the resolver's :id as the pstats key."
  [index {:biff.admin/keys [pstats]}]
  (if-not pstats
    index
    (let [wrap-resolve (fn [resolver]
                         (let [id (:id resolver)
                               orig-resolve (:resolve resolver)]
                           (assoc resolver :resolve
                                  (fn [ctx input]
                                    (let [[result pstats-data]
                                          (tufte/profiled {}
                                            (tufte/p (keyword (str id))
                                              (orig-resolve ctx input)))]
                                      (when pstats-data
                                        (swap! pstats (fn [existing]
                                                        (if existing
                                                          (tufte/merge-pstats existing pstats-data)
                                                          pstats-data))))
                                      result)))))]
      (-> index
          (update :all-resolvers #(mapv wrap-resolve %))
          (update :resolvers-by-output
                  (fn [m]
                    (reduce-kv (fn [acc k resolvers]
                                 (assoc acc k (mapv wrap-resolve resolvers)))
                               {}
                               m)))))))

;; ============================================================
;; Business metrics
;; ============================================================

(defn- day-key [instant]
  (str (t/date (t/in instant (t/zone "UTC")))))

(defn- compute-dau
  "Compute Daily Active Users from user events.
   Returns a sorted map of date-string -> count."
  [events]
  (->> events
       (group-by (fn [{:keys [instant]}] (day-key instant)))
       (reduce-kv (fn [m day evts]
                    (assoc m day (count (set (map :user-id evts)))))
                  (sorted-map))))

(defn- compute-wau
  "Compute Weekly Active Users (rolling 7-day window).
   For each day, count unique users from that day and previous 6 days."
  [events]
  (let [events-by-day (->> events
                           (group-by (fn [{:keys [instant]}] (day-key instant))))
        all-days (sort (keys events-by-day))]
    (when (seq all-days)
      (let [in-window? (fn [start-date end-date day-str]
                         (let [d (t/date day-str)]
                           (and (not (t/< d start-date))
                                (not (t/> d end-date)))))
            date-range (fn [end-date-str]
                         (let [end-date (t/date end-date-str)
                               start-date (t/- end-date (t/of-days 6))]
                           (filter #(in-window? start-date end-date %) all-days)))]
        (into (sorted-map)
              (map (fn [day]
                     (let [window-days (date-range day)
                           unique-users (->> window-days
                                             (mapcat #(get events-by-day %))
                                             (map :user-id)
                                             set
                                             count)]
                       [day unique-users])))
              all-days)))))

(defn- compute-daily-revenue
  "Compute daily revenue from revenue events."
  [events]
  (->> events
       (group-by (fn [{:keys [instant]}] (day-key instant)))
       (reduce-kv (fn [m day evts]
                    (assoc m day (reduce + 0 (map :revenue evts))))
                  (sorted-map))))

;; ============================================================
;; Performance metrics
;; ============================================================

(defn- format-pstats
  "Format pstats into a presentable data structure."
  [pstats-data]
  (when pstats-data
    (let [stats (tufte/format-pstats pstats-data)]
      stats)))

;; ============================================================
;; Admin routes
;; ============================================================

(defn- check-admin-access
  "Returns nil if access is allowed, or a response map if denied."
  [{:biff.admin/keys [user-id] :keys [session]}]
  (let [current-uid (str (:uid session))
        admin-uid (when (and user-id (not (str/blank? (str user-id))))
                    (str user-id))]
    (cond
      (nil? admin-uid)
      nil ;; allow but show UID display page

      (str/blank? current-uid)
      {:status 403 :headers {"content-type" "text/html"} :body "<h1>Forbidden</h1>"}

      (not= current-uid admin-uid)
      {:status 403 :headers {"content-type" "text/html"} :body "<h1>Forbidden</h1>"}

      :else
      nil)))

(defn- admin-uid-page
  "Page shown when BIFF_ADMIN_USER_ID is not set. Shows current user's UID."
  [{:keys [session]}]
  (let [uid (str (:uid session))]
    (ui/admin-page "Admin Setup"
      [:div.max-w-xl.mx-auto
       [:h1.text-2xl.font-bold.mb-4 "Admin Setup"]
       [:p.mb-4 "BIFF_ADMIN_USER_ID is not set. Your current user ID is:"]
       [:div.flex.items-center.gap-2.mb-4
        [:code.bg-gray-100.p-2.rounded.text-sm.break-all {:id "uid-display"} uid]
        [:button.bg-blue-600.text-white.px-3.py-1.rounded.text-sm.cursor-pointer
         {:onclick (str "navigator.clipboard.writeText('" uid "');"
                        "this.textContent='Copied!';"
                        "setTimeout(()=>this.textContent='Copy',2000)")}
         "Copy"]]
       [:p.text-sm.text-gray-600
        "Set this value as BIFF_ADMIN_USER_ID to enable the admin dashboard."]])))

(defn- admin-dashboard
  [{:biff.admin/keys [pstats get-user-events get-revenue-events user-id]
    :as ctx}]
  (if-not (and user-id (not (str/blank? (str user-id))))
    (admin-uid-page ctx)
    (let [user-events (when get-user-events (get-user-events ctx))
          revenue-events (when get-revenue-events (get-revenue-events ctx))
          dau (compute-dau (or user-events []))
          wau (compute-wau (or user-events []))
          daily-revenue (when revenue-events (compute-daily-revenue revenue-events))
          pstats-data (when pstats @pstats)
          pstats-formatted (format-pstats pstats-data)
          ;; Get total signups count from user events (unique users)
          total-signups (count (set (map :user-id (or user-events []))))
          ;; Get last 30 days of metrics
          today (str (t/date (t/in (t/now) (t/zone "UTC"))))
          recent-days (->> (keys dau) (take-last 30))]
      (ui/admin-page "Admin Dashboard"
        [:div
         [:h1.text-2xl.font-bold.mb-6 "Admin Dashboard"]

         ;; Business Metrics
         [:div.mb-8
          [:h2.text-xl.font-semibold.mb-4 "Business Metrics"]
          [:div.grid.grid-cols-2.md:grid-cols-4.gap-4.mb-6
           [:div.bg-white.p-4.rounded.border.border-gray-200
            [:div.text-sm.text-gray-600 "DAU (today)"]
            [:div.text-2xl.font-bold (get dau today 0)]]
           [:div.bg-white.p-4.rounded.border.border-gray-200
            [:div.text-sm.text-gray-600 "WAU (today)"]
            [:div.text-2xl.font-bold (get wau today 0)]]
           [:div.bg-white.p-4.rounded.border.border-gray-200
            [:div.text-sm.text-gray-600 "Total Signups"]
            [:div.text-2xl.font-bold total-signups]]
           (when daily-revenue
             [:div.bg-white.p-4.rounded.border.border-gray-200
              [:div.text-sm.text-gray-600 "Revenue (today)"]
              [:div.text-2xl.font-bold (format "$%.2f" (double (get daily-revenue today 0)))]])]

          ;; Recent metrics table
          (when (seq recent-days)
            [:details.mb-4
             [:summary.cursor-pointer.text-blue-600 "Daily metrics (last 30 days)"]
             [:table.w-full.mt-2.text-sm
              [:thead
               [:tr
                [:th.text-left.p-1 "Date"]
                [:th.text-right.p-1 "DAU"]
                [:th.text-right.p-1 "WAU"]
                (when daily-revenue [:th.text-right.p-1 "Revenue"])]]
              [:tbody
               (for [day (reverse recent-days)]
                 [:tr {:key day}
                  [:td.p-1 day]
                  [:td.text-right.p-1 (get dau day 0)]
                  [:td.text-right.p-1 (get wau day 0)]
                  (when daily-revenue
                    [:td.text-right.p-1 (format "$%.2f" (double (get daily-revenue day 0)))])])]]])]

         ;; Performance Metrics
         [:div.mb-8
          [:h2.text-xl.font-semibold.mb-4 "Performance Metrics"]
          (if pstats-formatted
            [:pre.bg-gray-100.p-4.rounded.text-xs.overflow-x-auto
             (str pstats-formatted)]
            [:p.text-gray-500 "No performance data collected yet."])]

         ;; User List
         [:div.mb-8
          [:h2.text-xl.font-semibold.mb-4 "Users"]
          (let [;; Get unique users with most recent activity
                users-by-recency (->> (or user-events [])
                                      (group-by :user-id)
                                      (map (fn [[uid evts]]
                                             {:user-id uid
                                              :last-active (apply max-key #(.toEpochMilli %)
                                                                  (map :instant evts))}))
                                      (sort-by #(- (.toEpochMilli (:last-active %)))))]
            (if (seq users-by-recency)
              [:div
               [:p.text-sm.text-gray-600.mb-2 (str (count users-by-recency) " users")]
               [:table.w-full.text-sm
                [:thead
                 [:tr
                  [:th.text-left.p-2.border-b "User ID"]
                  [:th.text-left.p-2.border-b "Last Active"]]]
                [:tbody
                 (for [{:keys [user-id last-active]} (take 50 users-by-recency)]
                   [:tr {:key (str user-id)}
                    [:td.p-2.border-b.font-mono.text-xs (str user-id)]
                    [:td.p-2.border-b (str (t/date (t/in last-active (t/zone "UTC"))))]])]]]
              [:p.text-gray-500 "No user data available."]))]]))))

(defn- wrap-admin-params
  "Middleware that merges admin parameters into the request."
  [handler params]
  (fn [ctx]
    (handler (merge ctx params))))

;; ============================================================
;; Module
;; ============================================================

(defn module
  "Creates a biff.admin module. Takes a map of options:
   - :biff.admin/get-user-events - fn [ctx] -> [{:user-id ... :instant ...} ...]
   - :biff.admin/get-revenue-events - (optional) fn [ctx] -> [{:revenue ... :instant ...} ...]
   - :biff.admin/get-route-id - (optional) fn [ctx] -> string, overrides default route ID extraction

   Returns a module map with :routes and :biff/init."
  [params]
  {:biff/init (fn [_modules-var]
                {:biff.admin/pstats (atom nil)})
   :routes ["/admin" {:middleware [(fn [handler]
                                    (wrap-admin-params handler params))
                                  (fn [handler]
                                    (fn [ctx]
                                      (if-let [resp (check-admin-access ctx)]
                                        resp
                                        (handler ctx))))]}
            ["" {:get admin-dashboard
                 :name ::dashboard}]]})
