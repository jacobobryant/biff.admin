(ns com.biffweb.admin-test
  (:require [clojure.test :refer [deftest is testing]]
            [com.biffweb.admin :as admin]
            [tick.core :as t]))

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
          middleware-fns (get-in routes [1 :middleware])
          ctx {:session {:uid "wrong-user"}
               :biff.admin/user-id "admin-user"
               :biff.admin/get-user-events (fn [_] [])}
          auth-middleware (second middleware-fns)
          wrapped (auth-middleware identity)
          resp (wrapped ctx)]
      (is (= 403 (:status resp)))))

  (testing "admin dashboard shows setup page when user-id not set"
    (let [m (admin/module {:biff.admin/get-user-events (fn [_] [])})
          routes (:routes m)
          middleware-fns (get-in routes [1 :middleware])
          ctx {:session {:uid "some-user"}
               :biff.admin/user-id nil
               :biff.admin/get-user-events (fn [_] [])}
          auth-middleware (second middleware-fns)
          wrapped (auth-middleware identity)
          resp (wrapped ctx)]
      (is (= 200 (:status resp)))
      (is (re-find #"biff.admin/user-id is not set" (:body resp)))))

  (testing "admin dashboard passes through when UIDs match"
    (let [m (admin/module {:biff.admin/get-user-events (fn [_] [])})
          routes (:routes m)
          middleware-fns (get-in routes [1 :middleware])
          ctx {:session {:uid "admin-user"}
               :biff.admin/user-id "admin-user"
               :biff.admin/get-user-events (fn [_] [])}
          auth-middleware (second middleware-fns)
          wrapped (auth-middleware (fn [_] {:status 200 :body "dashboard"}))
          resp (wrapped ctx)]
      (is (= 200 (:status resp)))
      (is (= "dashboard" (:body resp))))))

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

(deftest wrap-resolver-profiling-test
  (testing "wraps resolver resolve function"
    (let [resolver {:id :test/resolver
                    :resolve (fn [_ctx input] {:result (:value input)})}
          wrapped (admin/wrap-resolver-profiling resolver)]
      (is (= :test/resolver (:id wrapped)))
      (is (fn? (:resolve wrapped)))))

  (testing "wrapped resolver returns correct result"
    (let [resolver {:id :test/resolver
                    :resolve (fn [_ctx input] {:result (:value input)})}
          wrapped (admin/wrap-resolver-profiling resolver)
          ctx {:biff.admin/pstats (atom nil)}
          result ((:resolve wrapped) ctx {:value 42})]
      (is (= {:result 42} result)))))

(deftest default-get-route-id-test
  (testing "returns nil when no route match"
    (is (nil? (admin/default-get-route-id {:request-method :get}))))

  (testing "uses route name when available"
    (is (= "GET :com.biffweb.admin-test/my-route"
           (admin/default-get-route-id
            {:request-method :get
             :reitit.core/match {:data {:name ::my-route}
                                 :template "/test"}}))))

  (testing "falls back to template when no name"
    (is (= "POST /stuff/:id"
           (admin/default-get-route-id
            {:request-method :post
             :reitit.core/match {:data {}
                                 :template "/stuff/:id"}})))))
