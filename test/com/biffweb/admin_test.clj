(ns com.biffweb.admin-test
  (:require [clojure.test :refer [deftest is testing]]
            [com.biffweb.admin :as admin]
            [tick.core :as t]))

(deftest module-test
  (testing "module returns expected keys"
    (let [m (admin/module {:biff.admin/get-user-events (fn [_] [])})]
      (is (contains? m :biff.core/init))
      (is (contains? m :biff.ring/routes))
      (is (= [admin/wrap-profiling] (:biff.ring/base-middleware m)))
      (is (= [admin/wrap-resolver-profiling] (:biff.graph/middleware m)))
      (is (fn? (:biff.core/init m)))))

  (testing "biff.core/init creates pstats and signin-codes atoms"
    (let [m (admin/module {:biff.admin/get-user-events (fn [_] [])})
          init-result ((:biff.core/init m) nil)]
      (is (contains? init-result :biff.admin/pstats))
      (is (instance? clojure.lang.Atom (:biff.admin/pstats init-result)))
      (is (contains? init-result :biff.admin/signin-codes))
      (is (instance? clojure.lang.Atom (:biff.admin/signin-codes init-result))))))

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

(deftest health-endpoint-test
  (testing "module routes include health endpoint"
    (let [m (admin/module {:biff.admin/get-user-events (fn [_] [])})
          routes (:biff.ring/routes m)
          ;; Routes structure: [prefix {middleware} ["/health" ...] ...]
          health-route (some (fn [r] (when (and (vector? r) (= "/health" (first r))) r))
                            (rest (rest routes)))]
      (is (some? health-route)))))

(deftest use-alerts-test
  (testing "use-alerts adds errors-atom and alert-state to ctx"
    (let [ctx {:biff.core/stop []}
          result (admin/use-alerts ctx)]
      (is (contains? result :biff.admin/errors-atom))
      (is (contains? result :biff.admin/alert-state))
      (is (instance? clojure.lang.Atom (:biff.admin/errors-atom result)))
      ;; Clean up
      (doseq [f (:biff.core/stop result)] (f)))))
