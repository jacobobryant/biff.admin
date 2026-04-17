(ns com.biffweb.admin-test
  (:require [clojure.test :refer [deftest is testing]]
            [com.biffweb.admin :as admin]))

(deftest module-test
  (testing "module returns expected keys"
    (let [m (admin/module {:biff.admin/get-user-events (fn [_] [])})]
      (is (contains? m :biff/init))
      (is (contains? m :routes))
      (is (fn? (:biff/init m)))))

  (testing "biff/init creates pstats atom"
    (let [m (admin/module {:biff.admin/get-user-events (fn [_] [])})
          init-result ((:biff/init m) nil)]
      (is (contains? init-result :biff.admin/pstats))
      (is (instance? clojure.lang.Atom (:biff.admin/pstats init-result)))))

  (testing "admin dashboard returns 403 when user-id set but doesn't match"
    (let [m (admin/module {:biff.admin/get-user-events (fn [_] [])})
          routes (:routes m)
          ;; Extract the dashboard handler
          admin-route (nth routes 2)
          dashboard-fn (:get (second admin-route))
          middleware-fns (get-in routes [1 :middleware])
          ;; Build ctx with wrong user
          ctx {:session {:uid "wrong-user"}
               :biff.admin/user-id "admin-user"
               :biff.admin/get-user-events (fn [_] [])}
          ;; Apply the auth middleware
          auth-middleware (second middleware-fns)
          wrapped (auth-middleware identity)
          resp (wrapped ctx)]
      (is (= 403 (:status resp))))))

(deftest wrap-profiling-test
  (testing "wrap-profiling passes through when no pstats"
    (let [handler (fn [_] {:status 200})
          wrapped (admin/wrap-profiling handler)
          resp (wrapped {:request-method :get})]
      (is (= 200 (:status resp)))))

  (testing "wrap-profiling passes through when no route-id"
    (let [handler (fn [_] {:status 200})
          wrapped (admin/wrap-profiling handler)
          resp (wrapped {:request-method :get
                         :biff.admin/pstats (atom nil)})]
      (is (= 200 (:status resp)))))

  (testing "wrap-profiling profiles when route-id available"
    (let [pstats-atom (atom nil)
          handler (fn [_] {:status 200})
          wrapped (admin/wrap-profiling handler)
          resp (wrapped {:request-method :get
                         :biff.admin/pstats pstats-atom
                         :reitit.core/match {:data {:name ::test-route}
                                             :template "/test"}})]
      (is (= 200 (:status resp))))))
