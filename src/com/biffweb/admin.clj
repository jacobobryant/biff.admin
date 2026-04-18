(ns com.biffweb.admin
  "Admin dashboard library for Biff web applications.

   Provides:
   - Performance metrics via tufte profiling
   - Business metrics (DAU, WAU, revenue)
   - User list
   - Middleware for HTTP handler profiling
   - Helper for wrapping biff.graph resolvers with profiling"
  (:require [com.biffweb.admin.impl.ui :as ui]
            [clojure.string :as str]
            [taoensso.tufte :as tufte]
            [tick.core :as t]))

;; ============================================================
;; Profiling helpers
;; ============================================================

(defn- profile!
  "Execute f (a zero-arg function) under tufte profiling with the given id.
   Merges the resulting pstats into the :biff.admin/pstats atom."
  [ctx id f]
  (let [pstats (:biff.admin/pstats ctx)]
    (if (and pstats id)
      (let [[result pstats-data]
            (tufte/profiled {} (tufte/p (keyword id) (f)))]
        (when pstats-data
          (swap! pstats (fn [existing]
                          (if existing
                            (tufte/merge-pstats existing pstats-data)
                            pstats-data))))
        result)
      (f))))

(defn default-get-route-id
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
  (fn [{:biff.admin/keys [get-route-id] :as ctx}]
    (let [route-id ((or get-route-id default-get-route-id) ctx)]
      (profile! ctx route-id #(handler ctx)))))

(defn wrap-resolver-profiling
  "Middleware function for biff.graph/build-index.
   Wraps a resolver's :resolve function with tufte profiling."
  [resolver]
  (let [id (:id resolver)
        orig-resolve (:resolve resolver)]
    (assoc resolver :resolve
           (fn [ctx input]
             (profile! ctx (str id) #(orig-resolve ctx input))))))

;; ============================================================
;; Business metrics
;; ============================================================

(defn- day-key
  "Convert an instant to a tick/date in the given timezone."
  [instant tz]
  (t/date (t/in instant tz)))

(defn- date-range
  "Returns a sequence of dates from start-date to end-date (inclusive)."
  [start-date end-date]
  (->> (iterate #(t/+ % (t/of-days 1)) start-date)
       (take-while #(t/<= % end-date))
       vec))

(defn- compute-dau
  "Compute Daily Active Users from user events.
   Returns a sorted map of date -> count.
   Filters out events older than 30 days."
  [events tz now]
  (let [today (t/date (t/in now tz))
        cutoff (t/- today (t/of-days 30))]
    (->> events
         (filter #(t/<= cutoff (day-key (:instant %) tz)))
         (group-by #(day-key (:instant %) tz))
         (reduce-kv (fn [m day evts]
                      (assoc m day (count (set (map :user-id evts)))))
                    (sorted-map)))))

(defn- compute-wau
  "Compute Weekly Active Users (rolling 7-day window).
   For each day in the last 30 days, count unique users from that day and previous 6 days.
   Filters out events older than 37 days."
  [events tz now]
  (let [today (t/date (t/in now tz))
        cutoff (t/- today (t/of-days 37))
        events (filter #(t/<= cutoff (day-key (:instant %) tz)) events)
        events-by-day (group-by #(day-key (:instant %) tz) events)
        end-date today
        start-date (t/- today (t/of-days 29))
        days (date-range start-date end-date)]
    (into (sorted-map)
          (map (fn [day]
                 (let [window-start (t/- day (t/of-days 6))
                       window-days (date-range window-start day)
                       unique-users (->> window-days
                                         (mapcat #(get events-by-day %))
                                         (keep :user-id)
                                         set
                                         count)]
                   [day unique-users])))
          days)))

(defn- compute-daily-revenue
  "Compute daily revenue from revenue events.
   Filters out events older than 30 days."
  [events tz now]
  (let [today (t/date (t/in now tz))
        cutoff (t/- today (t/of-days 30))]
    (->> events
         (filter #(t/<= cutoff (day-key (:instant %) tz)))
         (group-by #(day-key (:instant %) tz))
         (reduce-kv (fn [m day evts]
                      (assoc m day (reduce + 0 (map :revenue evts))))
                    (sorted-map)))))

;; ============================================================
;; Admin routes
;; ============================================================

(defn- wrap-admin-access
  "Middleware that checks admin access. Shows setup page if :biff.admin/user-id
   is not set, returns 403 if UIDs don't match, otherwise calls handler."
  [handler]
  (fn [{:biff.admin/keys [user-id] :keys [session] :as ctx}]
    (let [current-uid (str (:uid session))
          admin-uid (str user-id)]
      (cond
        (str/blank? admin-uid)
        (ui/admin-page "Admin Setup"
          [:div
           (ui/heading "Admin Setup")
           [:p.mb-4 ":biff.admin/user-id is not set. Your current user ID is:"]
           [:div.flex.items-center.gap-2.mb-4
            [:code.bg-gray-100.p-2.rounded.text-sm.break-all {:id "uid-display"} current-uid]
            [:button.bg-blue-600.text-white.px-3.py-1.rounded.text-sm.cursor-pointer
             {:onclick (str "navigator.clipboard.writeText('" current-uid "');"
                            "this.textContent='Copied!';"
                            "setTimeout(()=>this.textContent='Copy',2000)")}
             "Copy"]]
           [:p.text-sm.text-gray-600
            "Set :biff.admin/user-id to enable the admin dashboard."]])

        (or (str/blank? current-uid)
            (not= current-uid admin-uid))
        {:status 403 :headers {"content-type" "text/html"} :body "<h1>Forbidden</h1>"}

        :else
        (handler ctx)))))

(defn- admin-dashboard-content
  [{:biff.admin/keys [pstats get-user-events get-revenue-events get-users]
    :as ctx}
   timezone]
  (let [tz (try (t/zone timezone) (catch Exception _ (t/zone "UTC")))
        now (t/now)
        user-events (when get-user-events (get-user-events ctx))
        revenue-events (when get-revenue-events (get-revenue-events ctx))
        dau (compute-dau (or user-events []) tz now)
        wau (compute-wau (or user-events []) tz now)
        daily-revenue (when revenue-events (compute-daily-revenue revenue-events tz now))
        pstats-data (when pstats @pstats)
        pstats-formatted (some-> pstats-data tufte/format-pstats)
        recent-days (->> (keys dau) (take-last 30))
        users (when get-users (get-users ctx))]
    (ui/admin-fragment
     [:div
      ;; Business Metrics
      (ui/section "Business Metrics"
        (when (seq recent-days)
          (ui/metrics-table recent-days dau wau daily-revenue))
        (when-not (seq recent-days)
          [:p.text-gray-500 "No activity data available."]))

      ;; Performance Metrics
      (ui/section "Performance Metrics"
        (if pstats-formatted
          [:pre.bg-gray-100.p-4.rounded.text-xs.overflow-x-auto
           (str pstats-formatted)]
          [:p.text-gray-500 "No performance data collected yet."]))

      ;; User List
      (ui/section "Users"
        (if (seq users)
          (ui/users-table users)
          [:p.text-gray-500 "No user data available."]))])))

(defn- admin-dashboard
  [ctx]
  (ui/admin-page "Admin"
    [:div
     (ui/heading "Admin")
     [:div {:id "admin-content"}
      [:p.text-gray-500 "Loading..."]]
     [:script
      (str "document.addEventListener('DOMContentLoaded', function() {"
           "  var tz = Intl.DateTimeFormat().resolvedOptions().timeZone || 'UTC';"
           "  fetch('/_biff/admin/content?timezone=' + encodeURIComponent(tz))"
           "    .then(function(r) { return r.text(); })"
           "    .then(function(html) { document.getElementById('admin-content').innerHTML = html; });"
           "});")]]))

(defn- admin-content-handler
  [ctx]
  (let [timezone (or (get-in ctx [:params :timezone])
                     (get-in ctx [:query-params "timezone"])
                     "UTC")]
    (admin-dashboard-content ctx timezone)))

(defn- wrap-admin-params
  "Middleware that merges admin parameters into the request.
   Used with Reitit vector syntax: [wrap-admin-params params]."
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
   - :biff.admin/get-users - (optional) fn [ctx] -> [{:user-id ... :joined-at ... :email ...} ...]
   - :biff.admin/get-route-id - (optional) fn [ctx] -> string, overrides default route ID extraction

   Returns a module map with :routes and :biff/init."
  [params]
  {:biff/init (fn [_modules-var]
                {:biff.admin/pstats (atom nil)})
   :routes ["/_biff/admin" {:middleware [[wrap-admin-params params]
                                        wrap-admin-access]}
            ["" {:get admin-dashboard
                 :name ::dashboard}]
            ["/content" {:get admin-content-handler
                         :name ::content}]]})
