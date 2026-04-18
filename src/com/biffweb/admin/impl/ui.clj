(ns com.biffweb.admin.impl.ui
  "UI helpers for the admin dashboard."
  (:require [lambdaisland.hiccup :as hiccup]))

(def ^:private datastar-version "1.0.0-beta.11")

(defn admin-page
  "Render an admin page with consistent layout, including datastar."
  [title & body]
  {:status 200
   :headers {"Content-Type" "text/html; charset=utf-8"}
   :body
   (str
    "<!DOCTYPE html>\n"
    (hiccup/render
     [:html {:lang "en"}
      [:head
       [:meta {:charset "utf-8"}]
       [:meta {:name "viewport"
               :content "width=device-width, initial-scale=1"}]
       [:title title]
       [:link {:rel "icon" :href "data:,"}]
       [:script {:src "https://cdn.tailwindcss.com"}]
       [:script {:type "module"
                 :src (str "https://cdn.jsdelivr.net/npm/@starfederation/datastar@"
                           datastar-version)}]]
      [:body.font-sans.bg-gray-50.text-gray-800.p-6.max-w-6xl.mx-auto
       body]]))})

(defn admin-fragment
  "Render an HTML fragment for datastar SSE merging."
  [hiccup]
  {:status 200
   :headers {"Content-Type" "text/html; charset=utf-8"}
   :body (str "<div id=\"admin-content\">"
              (hiccup/render hiccup)
              "</div>")})

(defn heading [text]
  [:h1.text-2xl.font-bold.mb-6 text])

(defn section [title & body]
  [:div.mb-8
   [:h2.text-xl.font-semibold.mb-4 title]
   body])

(defn metrics-table
  "Render a table of daily metrics."
  [days dau wau daily-revenue]
  [:div.overflow-x-auto
   [:table.w-full.text-sm
    [:thead
     [:tr
      [:th.text-left.p-2.border-b "Date"]
      [:th.text-right.p-2.border-b "DAU"]
      [:th.text-right.p-2.border-b "WAU"]
      (when daily-revenue [:th.text-right.p-2.border-b "Revenue"])]]
    [:tbody
     (for [day (reverse days)]
       [:tr {:key (str day)}
        [:td.p-2.border-b (str day)]
        [:td.text-right.p-2.border-b (get dau day 0)]
        [:td.text-right.p-2.border-b (get wau day 0)]
        (when daily-revenue
          [:td.text-right.p-2.border-b (format "$%.2f" (double (get daily-revenue day 0)))])])]]])

(defn users-table
  "Render a paginated table of users."
  [users]
  [:div
   [:p.text-sm.text-gray-600.mb-2 (str (count users) " users")]
   [:table.w-full.text-sm
    [:thead
     [:tr
      [:th.text-left.p-2.border-b "Email"]
      [:th.text-left.p-2.border-b "User ID"]
      [:th.text-left.p-2.border-b "Joined"]]]
    [:tbody
     (for [{:keys [user-id joined-at email]} (take 50 users)]
       [:tr {:key (str user-id)}
        [:td.p-2.border-b (or email "—")]
        [:td.p-2.border-b.font-mono.text-xs (str user-id)]
        [:td.p-2.border-b (str joined-at)]])]]])
