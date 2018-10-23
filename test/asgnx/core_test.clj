(ns asgnx.core-test
  (:require [clojure.test :refer :all]
            [clojure.core.async :refer [<!!]]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [clojure.test.check.generators :as gen]
            [asgnx.core :refer :all]
            [asgnx.kvstore :as kvstore :refer [put! get!]]))



(deftest words-test
  (testing "that sentences can be split into their constituent words"
    (is (= ["a" "b" "c"] (words "a b c")))
    (is (= [] (words "   ")))
    (is (= [] (words nil)))
    (is (= ["a"] (words "a")))
    (is (= ["a"] (words "a ")))
    (is (= ["a" "b"] (words "a b")))))


(deftest cmd-test
  (testing "that commands can be parsed from text messages"
    (is (= "foo" (cmd "foo")))
    (is (= "foo" (cmd "foo x y")))
    (is (= nil   (cmd nil)))
    (is (= ""    (cmd "")))))


(deftest args-test
  (testing "that arguments can be parsed from text messages"
    (is (= ["x" "y"] (args "foo x y")))
    (is (= ["x"] (args "foo x")))
    (is (= [] (args "foo")))
    (is (= [] (args nil)))))


(deftest parsed-msg-test
  (testing "that text messages can be parsed into cmd/args data structures"
    (is (= {:cmd "foo"
            :args ["x" "y"]}
           (parsed-msg "foo x y")))
    (is (= {:cmd "foo"
            :args ["x"]}
           (parsed-msg "foo x")))
    (is (= {:cmd "foo"
            :args []}
           (parsed-msg "foo")))
    (is (= {:cmd "foo"
            :args ["x" "y" "z" "somereallylongthing"]}
           (parsed-msg "foo x y z somereallylongthing")))))

(deftest welcome-test
  (testing "that welcome messages are correctly formatted"
    (is (= "Welcome bob" (welcome {:cmd "welcome" :args ["bob"]})))
    (is (= "Welcome bob" (welcome {:cmd "welcome" :args ["bob" "smith"]})))
    (is (= "Welcome bob smith jr" (welcome {:cmd "welcome" :args ["bob smith jr"]})))))


(deftest homepage-test
  (testing "that the homepage is output correctly"
    (is (= cs4278-brightspace (homepage {:cmd "homepage" :args []})))))


(deftest format-hour-test
  (testing "that 0-23 hour times are converted to am/pm correctly"
    (is (= "1am" (format-hour 1)))
    (is (= "1pm" (format-hour 13)))
    (is (= "2pm" (format-hour 14)))
    (is (= "12am" (format-hour 0)))
    (is (= "12pm" (format-hour 12)))))


(deftest formatted-hours-test
  (testing "that the office hours data structure is correctly converted to a string"
    (is (= "from 8am to 10am in the chairs outside of the Wondry"
           (formatted-hours {:start 8 :end 10 :location "the chairs outside of the Wondry"})))
    (is (= "from 4am to 2pm in the chairs outside of the Wondry"
           (formatted-hours {:start 4 :end 14 :location "the chairs outside of the Wondry"})))
    (is (= "from 2pm to 10pm in the chairs outside of the Wondry"
           (formatted-hours {:start 14 :end 22 :location "the chairs outside of the Wondry"})))))


(deftest office-hours-for-day-test
  (testing "testing lookup of office hours on a specific day"
    (is (= "from 8am to 10am in the chairs outside of the Wondry"
           (office-hours {:cmd "office hours" :args ["thursday"]})))
    (is (= "from 8am to 10am in the chairs outside of the Wondry"
           (office-hours {:cmd "office hours" :args ["tuesday"]})))
    (is (= "there are no office hours on that day"
           (office-hours {:cmd "office" :args ["wednesday"]})))
    (is (= "there are no office hours on that day"
           (office-hours {:cmd "office" :args ["monday"]})))))


(deftest create-router-test
  (testing "correct creation of a function to lookup a handler for a parsed message"
    (let [router (create-router {"hello" #(str (:cmd %) " " "test")
                                 "argc"  #(count (:args %))
                                 "echo"  identity
                                 "default" (fn [& a] "No!")})
          msg1   {:cmd "hello"}
          msg2   {:cmd "argc" :args [1 2 3]}
          msg3   {:cmd "echo" :args ["a" "z"]}
          msg4   {:cmd "echo2" :args ["a" "z"]}]
      (is (= "hello test" ((router msg1) msg1)))
      (is (= "No!" ((router msg4) msg4)))
      (is (= 3 ((router msg2) msg2)))
      (is (= msg3 ((router msg3) msg3))))))


(deftest action-send-msg-test
  (testing "That action send msg returns a correctly formatted map"
    (is (= :send
           (:action (action-send-msg :bob "foo"))))
    (is (= :bob
           (:to (action-send-msg :bob "foo"))))
    (is (= "foo"
           (:msg (action-send-msg [:a :b] "foo"))))))


(deftest action-send-msgs-test
  (testing "That action send msgs generates a list of sends"
    (let [a (action-send-msg [:a :f :b] 1)
          b (action-send-msg [:a :f :d] 1)
          c (action-send-msg [:a :f :e] 1)
          d (action-send-msg [:a :f :c] 1)]
      (is (= [a b c d]
             (action-send-msgs [[:a :f :b]
                                [:a :f :d]
                                [:a :f :e]
                                [:a :f :c]]
                              1))))))

(deftest action-insert-test
  (testing "That action insert returns a correctly formatted map"
    (is (= #{:action :ks :v}
           (into #{}(keys (action-insert [:a :b] {:foo 1})))))
    (is (= #{:assoc-in [:a :b] {:foo 1}}
           (into #{}(vals (action-insert [:a :b] {:foo 1})))))
    (is (= :assoc-in
           (:action (action-insert [:a :b] {:foo 1}))))
    (is (= {:foo 1}
           (:v (action-insert [:a :b] {:foo 1}))))
    (is (= [:a :b]
           (:ks (action-insert [:a :b] {:foo 1}))))))


(deftest action-remove-test
  (testing "That action remove returns a correctly formatted map"
    (is (= #{:action :ks}
         (into #{} (keys (action-remove [:a :b])))))
    (is (= #{:dissoc-in [:a :b]}
          (into #{}(vals (action-remove [:a :b])))))
    (is (= :dissoc-in
           (:action (action-remove [:a :b]))))
    (is (= [:a :b]
           (:ks (action-remove [:a :b]))))))


(deftest action-inserts-test
  (testing "That action inserts generates a list of inserts"
    (let [a (action-insert [:a :f :b] 1)
          b (action-insert [:a :f :d] 1)
          c (action-insert [:a :f :e] 1)
          d (action-insert [:a :f :c] 1)]
      (is (= [a b c d]
             (action-inserts [:a :f] [:b :d :e :c] 1))))))


(defn action-send [system {:keys [to msg]}]
  (put! (:state-mgr system) [:msgs to] msg))

(defn pending-send-msgs [system to]
  (get! (:state-mgr system) [:msgs to]))

(def send-action-handlers
  {:send action-send})

(deftest handle-message-test
  (testing "the integration and handling of messages"
    (let [ehdlrs (merge
                   send-action-handlers
                   kvstore/action-handlers)
          state  (atom {})
          smgr   (kvstore/create state)
          system {:state-mgr smgr
                  :effect-handlers ehdlrs}]
      (is (= "You must enter a valid name."
             (<!! (handle-message
                    system
                    "test-user"
                    "register"))))
      (is (= "test-user has been added."
             (<!! (handle-message
                    system
                    "test-user"
                    "register firstname"))))
      ;; "User is not registered" needs only to be testd once
      ;; although it appears in other functions because the
      ;; implementation is the same for the first few functions.
      (is (= "User is not registered."
             (<!! (handle-message
                    system
                    "not-test-user"
                    "name firstname"))))
      (is (= "You must enter a name."
             (<!! (handle-message
                    system
                    "test-user"
                    "name"))))
      ;; The same goes for "invalid topics".
      (is (= "Invalid topics."
             (<!! (handle-message
                    system
                    "test-user"
                    "subscribe technology random"))))
      (is (= "test-user has subscribed to: technology, business."
             (<!! (handle-message
                    system
                    "test-user"
                    "subscribe technology business"))))
      (is (= "test-user has unsubscribed from: business, technology."
             (<!! (handle-message
                    system
                    "test-user"
                    "unsubscribe business technology"))))
      (is (= "You must enter a number less than or equal to 10."
             (<!! (handle-message
                    system
                    "test-user"
                    "quantity 11"))))
      (is (= "test-user has updated news quantity to: 2."
             (<!! (handle-message
                   system
                   "test-user"
                   "quantity 2"))))
      (is (= "You must enter 'yes' or 'no'."
             (<!! (handle-message
                   system
                   "test-user"
                   "content yes no"))))
      (is (= "test-user has changed content setting to true."
             (<!! (handle-message
                   system
                   "test-user"
                   "content yes"))))
      (is (= "test-user has changed content setting to false."
             (<!! (handle-message
                   system
                   "test-user"
                   "content no"))))
      (is (= "You must enter 'yes' or 'no'."
             (<!! (handle-message
                   system
                   "test-user"
                   "image yesandno"))))
      (is (= "test-user has changed image setting to true."
             (<!! (handle-message
                   system
                   "test-user"
                   "image yes"))))
      (is (= "test-user has changed image setting to false."
             (<!! (handle-message
                   system
                   "test-user"
                   "image no"))))
      (is (= "Invalid command."
             (<!! (handle-message
                   system
                   "test-user"
                   "update"))))
      (is (= "Daily update complete."
             (<!! (handle-message
                   system
                   "9089388920"
                   "update"))))
      (is (= "Invalid command."
             (<!! (handle-message
                   system
                   "test-user"
                   "send"))))
      (is (= "Articles successfully sent."
             (<!! (handle-message
                   system
                   "9089388920"
                   "send"))))
      ;; This result varies, it will be the last message sent to the user since multiple messages are sent for each article
      ;; The result will also depend on the article itself, which may vary from day to day
      ;; THIS SHOULD "FAIL", check the result manually
      (is (= ""
             (<!! (pending-send-msgs system "test-user"))))
      (is (= "User has not been registered."
             (<!! (handle-message
                   system
                   "not-test-user"
                   "newstopic"))))
      (is (= "You must enter one topic."
             (<!! (handle-message
                   system
                   "test-user"
                   "newstopic technology business"))))
      (is (= "You must enter a valid topic."
             (<!! (handle-message
                   system
                   "test-user"
                   "newstopic random"))))
      (is (= "News for topic: technology."
             (<!! (handle-message
                   system
                   "test-user"
                   "newstopic technology"))))
      ;; This result again varies, it depends on the last message sent and the article searched for
      ;; THIS SHOULD "FAIL", check result manually
      (is (= ""
             (<!! (pending-send-msgs system "test-user"))))
      (is (= "User has not been registered."
             (<!! (handle-message
                   system
                   "not-test-user"
                   "newsquery something"))))
      (is (= "You must enter a query."
             (<!! (handle-message
                   system
                   "test-user"
                   "newsquery"))))
      (is (= "News for query: jeff bezos."
             (<!! (handle-message
                   system
                   "test-user"
                   "newsquery jeff bezos"))))
      ;; This result also varies, it depends on the last message sent and the article queried
      ;; THIS SHOULD "FAIL", check result manually
      (is (= ""
             (<!! (pending-send-msgs system "test-user")))))))
