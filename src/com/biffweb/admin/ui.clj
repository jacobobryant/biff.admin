(ns com.biffweb.admin.ui
  "UI helpers for the admin dashboard."
  (:require [rum.core :as rum]))

(defn admin-page
  "Render an admin page with consistent layout."
  [title & body]
  {:status 200
   :headers {"Content-Type" "text/html; charset=utf-8"}
   :body
   (str
    "<!DOCTYPE html>\n"
    (rum/render-static-markup
     [:html {:lang "en"}
      [:head
       [:meta {:charset "utf-8"}]
       [:meta {:name "viewport"
               :content "width=device-width, initial-scale=1"}]
       [:title title]
       [:link {:rel "icon" :href "data:,"}]
       [:script {:src "https://cdn.tailwindcss.com"}]]
      [:body.font-sans.bg-gray-50.text-gray-800.p-6.max-w-6xl.mx-auto
       body]]))})
