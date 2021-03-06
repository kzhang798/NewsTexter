(ns asgnx.core
  (:require [clojure.string :as string]
            [clojure.core.async :as async :refer [go chan <! >!]]
            [asgnx.kvstore :as kvstore
             :refer [put! get! list! remove!]]
            [clj-http.client :as client]
            [asgnx.json :as json]))


;; Do not edit!
;; A def for the course home page URL.
(def cs4278-brightspace "https://brightspace.vanderbilt.edu/d2l/home/85892")


;; Do not edit!
;; A map specifying the instructor's office hours that is keyed by day of the week.
(def instructor-hours {"tuesday"  {:start    8
                                   :end      10
                                   :location "the chairs outside of the Wondry"}

                       "thursday" {:start    8
                                   :end      10
                                   :location "the chairs outside of the Wondry"}})


;; This is a helper function that you might want to use to implement
;; `cmd` and `args`.
(defn words [msg]
  (if msg
    (string/split msg #" ")
    []))

;; Asgn 1.
;;
;; @Todo: Fill in this function to return the first word in a text
;; message.
;;
;; Example: (cmd "foo bar") => "foo"
;;
;; See the cmd-test in test/asgnx/core_test.clj for the
;; complete specification.
;;
(defn cmd [msg]
  (def arr
    (words msg))
  (if arr
    (first arr)
    []))


;; Asgn 1.
;;
;; @Todo: Fill in this function to return the list of words following
;; the command in a text message.
;;
;; Example: (args "foo bar baz") => ("bar" "baz")
;;
;; See the args-test in test/asgnx/core_test.clj for the
;; complete specification.
;;
(defn args [msg]
  (def arr
    (words msg))
  (if arr
    (rest arr)
    []))

;; Asgn 1.
;;
;; @Todo: Fill in this function to return a map with keys for the
;; :cmd and :args parsed from the msg.
;;
;; Example:
;;
;; (parsed-msg "foo bar baz") => {:cmd "foo" :args ["bar" "baz"]}
;;
;; See the parsed-msg-test in test/asgnx/core_test.clj for the
;; complete specification.
;;
(defn parsed-msg [msg]
  {:cmd (cmd msg)
   :args (args msg)})

;; Asgn 1.
;;
;; @Todo: Fill in this function to prefix the first of the args
;; in a parsed message with "Welcome " and return the result.
;;
;; Example:
;;
;; (welcome {:cmd "welcome" :args ["foo"]}) => "Welcome foo"
;;
;; See the welcome-test in test/asgnx/core_test.clj for the
;; complete specification.
;;
(defn welcome [pmsg]
  (str "Welcome " (string/join (first (:args pmsg)))))

;; Asgn 1.
;;
;; @Todo: Fill in this function to return the CS 4278 home page.
;; Use the `cs4278-brightspace` def to produce the output.
;;
;; See the homepage-test in test/asgnx/core_test.clj for the
;; complete specification.
;;
(defn homepage [_]
  cs4278-brightspace)

;; Asgn 1.
;;
;; @Todo: Fill in this function to convert from 0-23hr format
;; to AM/PM format.
;;
;; Example: (format-hour 14) => "2pm"
;;
;; See the format-hour-test in test/asgnx/core_test.clj for the
;; complete specification.
;;
(defn format-hour [h]
  (cond
    (= h 0) "12am"
    (= h 12) "12pm"
    (> h 12) (str (- h 12) "pm")
    (< h 12) (str h "am")))

;; Asgn 1.
;;
;; @Todo: This function should take a map in the format of
;; the values in the `instructor-hours` map (e.g. {:start ... :end ... :location ...})
;; and convert it to a string format.
;;
;; Example:
;; (formatted-hours {:start 8 :end 10 :location "the chairs outside of the Wondry"}))
;; "from 8am to 10am in the chairs outside of the Wondry"
;;
;; You should use your format-hour function to implement this.
;;
;; See the formatted-hours-test in test/asgnx/core_test.clj for the
;; complete specification.
;;
(defn formatted-hours [hours]
  (def start (format-hour (:start hours)))
  (def end (format-hour (:end hours)))
  (def location (:location hours))
  (str "from " start " to " end " in " location))

;; Asgn 1.
;;
;; @Todo: This function should lookup and see if the instructor
;; has office hours on the day specified by the first of the `args`
;; in the parsed message. If so, the function should return the
;; `formatted-hours` representation of the office hours. If not,
;; the function should return "there are no office hours on that day".
;; The office hours for the instructor should be obtained from the
;; `instructor-hours` map.
;;
;; You should use your formatted-hours function to implement this.
;;
;; See the office-hours-for-day-test in test/asgnx/core_test.clj for the
;; complete specification.
;;
(defn office-hours [{:keys [args cmd]}]
  (def officeHours (get instructor-hours (first args)))
  (if officeHours
    (formatted-hours officeHours)
    "there are no office hours on that day"))

;; Asgn 2.
;;
;; @Todo: Create a function called action-send-msg that takes
;; a destination for the msg in a parameter called `to`
;; and the message in a parameter called `msg` and returns
;; a map with the keys :to and :msg bound to each parameter.
;; The map should also have the key :action bound to the value
;; :send.
;;
(defn action-send-msg [to msg]
  {:action :send
   :to to
   :msg msg})

;; Asgn 2.
;;
;; @Todo: Create a function called action-send-msgs that takes
;; takes a list of people to receive a message in a `people`
;; parameter and a message to send them in a `msg` parmaeter
;; and returns a list produced by invoking the above `action-send-msg`
;; function on each person in the people list.
;;
;; java-like pseudo code:
;;
;; output = new list
;; for person in people:
;;   output.add( action-send-msg(person, msg) )
;; return output
;;
(defn action-send-msgs [people msg]
  (for [person people] (action-send-msg person msg)))

;; Asgn 2.
;;
;; @Todo: Create a function called action-insert that takes
;; a list of keys in a `ks` parameter, a value to bind to that
;; key path to in a `v` parameter, and returns a map with
;; the key :ks bound to the `ks` parameter value and the key :v
;; vound to the `v` parameter value.)
;; The map should also have the key :action bound to the value
;; :assoc-in.
;;
(defn action-insert [ks v]
  (let [result {:action :assoc-in
                :ks ks
                :v v}]
    result))

;; Asgn 2.
;;
;; @Todo: Create a function called  that takes:
;; 1. a key prefix (e.g., [:a :b])
;; 2. a list of suffixes for the key (e.g., [:c :d])
;; 3. a value to bind
;;
;; and calls (action-insert combined-key value) for each possible
;; combined-key that can be produced by appending one of the suffixes
;; to the prefix.
;;
;; In other words, this invocation:
;;
;; ( [:foo :bar] [:a :b :c] 32)
;;
;; would be equivalent to this:
;;
;; [(action-insert [:foo :bar :a] 32)
;;  (action-insert [:foo :bar :b] 32)
;;  (action-insert [:foo :bar :c] 32)]
;;
(defn action-inserts [prefix ks v]
  (for [k ks] (let [c (concat prefix [k])
                    result (action-insert c v)]
                result)))

;; Asgn 2.
;;
;; @Todo: Create a function called action-remove that takes
;; a list of keys in a `ks` parameter and returns a map with
;; the key :ks bound to the `ks` parameter value.
;; The map should also have the key :action bound to the value
;; :dissoc-in.
;;
(defn action-remove [ks]
  {:action :dissoc-in
   :ks ks})


;; Asgn 3.
;;
;; @Todo: Create a function called "experts-register"
;; that takes the current application `state`, a `topic`
;; the expert's `id` (e.g., unique name), and information
;; about the expert (`info`) and registers them as an expert on
;; the specified topic. Look at the associated test to see the
;; expected function signature.
;;
;; Your function should NOT directly change the application state
;; to register them but should instead return a list of the
;; appropriate side-effects (above) to make the registration
;; happen.
;;
;; See the integration test in See handle-message-test for the
;; expectations on how your code operates
;;
(defn experts-register [experts topic id info]
  [(action-insert [:expert topic] {id info})])

;; Asgn 3.
;;
;; @Todo: Create a function called "experts-unregister"
;; that takes the current application `state`, a `topic`
;; and the expert's `id` (e.g., unique name) and then
;; removes the expert from the list of experts on that topic.
;; Look at the associated test to see the expected function signature.
;;
;; Your function should NOT directly change the application state
;; to unregister them but should instead return a list of the
;; appropriate side-effects (above) to make the registration
;; happen.
;;
;; See the integration test in See handle-message-test for the
;; expectations on how your code operates
;;
(defn experts-unregister [experts topic id]
  [(action-remove [:experts topic id])])

(defn experts-question-msg [experts question-words]
  (str "Asking " (count experts) " expert(s) for an answer to: \""
       (string/join " " question-words) "\""))

;; Asgn 3.
;;
;; @Todo: Create a function called "ask-experts"
;; that takes two parameters:
;;
;; 1. the list of experts on the topic
;; 2. a parsed message with the format:
;;    {:cmd "ask"
;;     :user-id "phone number that sent the message"
;;     :args [topic question-word1 question-word2 ... question-wordN]}
;;
;; The sender of the message will be identified by their phone number
;; in the user-id parameter. This is the phone number that you will need
;; to forward answers to the question to.
;;
;; The parsed message is generated by breaking up the words in the ask
;; text message. For example, if someone sent the message:
;;
;; "ask food what is the best pizza in nashville"
;;
;; The parsed message would be:
;;
;; {:cmd "ask"
;;  :user-id "+15555555555"
;;  :args ["food" "what" "is" "the" "best" "pizza" "in" "nashville"]}
;;
;; This function needs to return a list with two elements:
;; [[actions...] "response to asker"]
;;
;; The actions in the list are the *side effects* that need to take place
;; to ask the question (e.g., sending messages to the experts). The string
;; is the response that is going to be sent back to the person that asked
;; the question (e.g. "Asking 2 expert(s) for an answer to ....").
;;
;; The correct string response to a valid question should be produced with
;; the `experts-question-msg` function above.
;;
;; Think about how you are going to figure out where to route messages
;; when an expert answers (see the conversations query) and make sure you
;; handle the needed side effect for storing the conversation state.
;;
;; If there are no registered experts on a topic, you should return an
;; empty list of actions and "There are no experts on that topic."
;;
;; If there isn't a question, you should return "You must ask a valid question."
;;
;; Why this strange architecture? By returning a list of the actions to take,
;; rather than directly taking that action, we can keep this function pure.
;; Pure functions are WAY easier to test / maintain. Also, we can isolate our
;; messy impure action handling at the "edges" of the application, where it is
;; easier to track and reason about.
;;
;; You should look at `handle-message` to get an idea of the way that this
;; function is going to be used, its expected signature, and how the actions
;; and output are going to work.
;;
;; See the integration test in See handle-message-test for the
;; expectations on how your code operates
;;
(defn ask-experts [experts {:keys [args user-id]}]
  (if (< 1 (count args))
    (if experts
      [(concat (action-send-msgs experts (string/join " " (rest args)))
               (action-inserts [:conversations] experts
                               {:last-question (rest args)
                                :asker user-id}))
       (experts-question-msg experts (rest args))]
      [[] "There are no experts on that topic."])
    [[] "You must ask a valid question."]))

;; Asgn 3.
;;
;; @Todo: Create a function called "answer-question"
;; that takes two parameters:
;;
;; 1. the last conversation describing the last question that was routed
;;    to the expert
;; 2. a parsed message with the format:
;;    {:cmd "ask"
;;     :user-id "+15555555555"
;;     :args [topic answer-word1 answer-word2 ... answer-wordN]}
;;
;; The parsed message is generated by breaking up the words in the ask
;; text message. For example, if someone sent the message:
;;
;; "answer joey's house of pizza"
;;
;; The conversation will be data that you store as a side-effect in
;; ask-experts. You probably want this data to be information about the
;; last question asked to each expert. See the "think about" comment above.
;;
;; The parsed message would be:
;;
;; {:cmd "answer"
;;  :user-id "+15555555555"
;;  :args ["joey's" "house" "of" "pizza"]}
;;
;; This function needs to return a list with two elements:
;; [[actions...] "response to expert answering"]
;;
;; The actions in the list are the *side effects* that need to take place
;; to send the answer to the original question asker. The string
;; is the response that is going to be sent back to the expert answering
;; the question.
;;
;; Think about how you are going to figure out where to route messages
;; when an expert answers (see the conversations query) and make sure you
;; handle the needed side effect for storing the conversation state.
;;
;; Why this strange architecture? By returning a list of the actions to take,
;; rather than directly taking that action, we can keep this function pure.
;; Pure functions are WAY easier to test / maintain. Also, we can isolate our
;; messy impure action handling at the "edges" of the application, where it is
;; easier to track and reason about.
;;
;; You should look at `handle-message` to get an idea of the way that this
;; function is going to be used, its expected signature, and how the actions
;; and output are going to work.
;;
;; See the integration test in See handle-message-test for)
;; expectations on how your code
;;
(defn answer-question [conversation {:keys [args]}]
  (if (< 1 (count args))
    (if conversation
      [[(action-send-msg (:asker conversation) (string/join " " args))]
       "Your answer was sent."]
      [[] "You haven't been asked a question."])
    [[] "You did not provide an answer."]))

;; Asgn 3.
;;
;; @Todo: Create a function called "add-expert"
;; that takes two parameters:
;;
;; 1. the current list of experts on the topic
;; 2. a parsed message with the format:
;;    {:cmd "expert"
;;     :user-id "+15555555555"
;;     :args [topic]
;;
;;
;; The parsed message is generated by breaking up the words in the expert
;; text message. For example, if someone sent the message:
;;
;; "expert food"
;;
;; The parsed message would be:
;;
;; {:cmd "expert"
;;  :user-id "+15555555555"))
;;  :args ["food"]}
;;
;; This function needs to add "sara" to the list of experts on "food" and
;; associate her phone number with her ID.
;;
;; This function needs to return a list with two elements:
;; [[actions...] "response to the person adding themselves as an expert"]
;;
;; The actions in the list are the *side effects* that need to take place
;; to add the person as an expert on the topic (hint: result of calling experts-register). The string
;; is the response that is going to be sent back to the person adding themselves
;; as an expert.
;;
;; You should look at `handle-message` to get an idea of the way that this
;; function is going to be used, its expected signature, and how the actions
;; and output are going to work.
;;
;; See the integration test in See handle-message-test for the
;; expectations on how your code operates
(defn add-expert [experts {:keys [args user-id]}]
  [[(action-insert [:expert (first args) user-id] "")]
   (str user-id " is now an expert on " (first args) ".")])

;; Don't edit!
(defn stateless [f]
  (fn [_ & args]
    [[] (apply f args)]))

;; Asgn 3.
;;
;; @Todo: Add mappings of the cmds "expert", "ask", and "answer" to
;; to the `routes` map so that the functions that you
;; created will be invoked when the corresponding text message
;; commands are received.
;;})


;; Don't edit!
(defn experts-on-topic-query [state-mgr pmsg]
  (let [[topic]  (:args pmsg)]
    (list! state-mgr [:expert topic])))


;; Don't edit!
(defn conversations-for-user-query [state-mgr pmsg]
  (let [user-id (:user-id pmsg)]
    (get! state-mgr [:conversations user-id])))

;; Functions for newstexter
(defn users-query [state-mgr pmsg]
  (get! state-mgr [:users]))

(defn user-info-query [state-mgr pmsg]
  (let [user-id (:user-id pmsg)]
    (get! state-mgr [:users user-id])))

(defn all-info-query [state-mgr pmsg]
  (get! state-mgr nil))

;; Don't edit!
(def queries
  {"expert" experts-on-topic-query
   "ask"    experts-on-topic-query
   "answer" conversations-for-user-query
   "register" users-query
   "name"   users-query
   "subscribe" users-query
   "unsubscribe" users-query
   "quantity" users-query
   "content" users-query
   "image"  users-query
   "update" all-info-query
   "send"   all-info-query
   "newstopic" all-info-query
   "newsquery" user-info-query})

;; End functions for newstexter

;; Don't edit!
(defn read-state [state-mgr pmsg]
  (go
    (if-let [qfn (get queries (:cmd pmsg))]
      (<! (qfn state-mgr pmsg))
      {})))


;; Asgn 1.
;;
;; @Todo: This function should return a function (<== pay attention to the
;; return type) that takes a parsed message as input and returns the
;; function in the `routes` map that is associated with a key matching
;; the `:cmd` in the parsed message. The returned function would return
;; `welcome` if invoked with `{:cmd "welcome"}`.
;;
;; Example:
;;
;; (let [msg {:cmd "welcome" :args ["bob"]}]
;;   (((create-router {"welcome" welcome}) msg) msg) => "Welcome bob"
;;
;; If there isn't a function in the routes map that is mapped to a
;; corresponding key for the command, you should return the function
;; mapped to the key "default".
;;
;; See the create-router-test in test/asgnx/core_test.clj for the
;; complete specification.
;;
(defn create-router [routes]
  (fn [parsedMsg]
    (if (routes (:cmd parsedMsg))
      (routes (:cmd parsedMsg))
      (routes "default"))))

;; Don't edit!
(defn output [o]
  (second o))


;; Don't edit!
(defn actions [o]
  (first o))


;; Don't edit!
(defn invoke [{:keys [effect-handlers] :as system} e]
  (go
    (println "    Invoke:" e)
    (if-let [action (get effect-handlers (:action e))]
      (do
        (println "    Invoking:" action "with" e)
        (<! (action system e))))))


;; Don't edit!
(defn process-actions [system actions]
  (go
    (println "  Processing actions:" actions)
    (let [results (atom [])]
      (doseq [action actions]
        (let [result (<! (invoke system action))]
          (swap! results conj result)))
      @results)))

;; Functions for newstexter

; (defn action-insert [ks v]
;   (let [result {:action :assoc-in
;                 :ks ks
;                 :v v}]
;     result))
;
; (defn action-inserts [prefix ks v]
;   (for [k ks] (let [c (concat prefix [k])
;                     result (action-insert c v)]
;                 result)))
;
; (defn action-remove [ks]
;   {:action :dissoc-in
;    :ks ks})

;; @FoundCode
;; https://stackoverflow.com/questions/5621279/in-clojure-how-can-i-convert-a-string-to-a-number

(defn parse-int [s]
   (Integer. (re-find  #"\d+" s)))

;; @EndFoundCode

;; Takes a prefix list and list of keys and returns a list of actions
;; to remove the concatenated prefixes and keys
(defn action-removes [prefix ks]
  (for [k ks] (action-remove (concat prefix [k]))))

(def topics #{"general"
              "technology"
              "sports"
              "politics"
              "business"
              "entertainment"
              "science"})

;; Returns a string of all the topics
(defn show-topics [_]
  (string/join "\n" topics))

;; Returns actions to register the user if the user inputs valid args
;; Gives the user the default settings and sets the name to first of args
(defn add-user [users {:keys [args user-id]}]
  (if (and (<= 1 (count args)) (not (string/blank? (first args))))
    [[(action-insert [:users user-id] {:name (first args)
                                       :topics {"general" ""}
                                       :quantity 3
                                       :content false
                                       :image false})]
     (str user-id " has been added.")]
    [[] "You must enter a valid name."]))

;; Returns actions to change the name of a user
(defn set-name [users {:keys [args user-id]}]
  (if (contains? users user-id)
    (if (and (<= 1 (count args)) (not (string/blank? (first args))))
      [[(action-insert [:users user-id :name] (string/join " " args))]
       (str user-id " has set name to: " (string/join " " args) ".")]
      [[] "You must enter a name."])
    [[] "User is not registered."]))

;; Returns actions to add topics to the map stored at key :topics of a user's settings,
;; subscribing them to topics
(defn subscribe [users {:keys [args user-id]}]
  (if (contains? users user-id)
    (if (and (<= 1 (count args)) (not (string/blank? (first args))))
      (if (every? topics args)
        [(action-inserts [:users user-id :topics] args "")
         (str user-id " has subscribed to: " (string/join ", " args) ".")]
        [[] "Invalid topics."])
      [[] "You must choose topics."])
    [[] "User is not registered."]))

;; Returns actions to remove topics from the map stored at key :topics of a user's settings,
;; unsubscribing them from topics
(defn unsubscribe [users {:keys [args user-id]}]
  (if (contains? users user-id)
    (if (and (<= 1 (count args)) (not (string/blank? (first args))))
      (if (every? topics args)
        [(action-removes [:users user-id :topics] args)
         (str user-id " has unsubscribed from: " (string/join ", " args) ".")]
        [[] "Invalid topics."])
      [[] "You must choose topics."])
    [[] "User is not registered."]))

;; Returns actions to set the quantity of news a user would like to see, max 10
(defn set-quantity [users {:keys [args user-id]}]
  (if (contains? users user-id)
    (if (and (= 1 (count args)) (not (string/blank? (first args))) (>= 10 (parse-int (first args))))
      [[(action-insert [:users user-id :quantity] (parse-int (first args)))]
       (str user-id " has updated news quantity to: " (first args) ".")]
      [[] "You must enter a number less than or equal to 10."])
    [[] "User is not registered."]))

;; Returns actions to set the option to see content about a news article
(defn set-content [users {:keys [args user-id]}]
  (if (contains? users user-id)
    (if (= 1 (count args))
      (cond
        (= "yes" (string/lower-case (first args))) [[(action-insert [:users user-id :content] true)]
                                                    (str user-id " has changed content setting to true.")]
        (= "no" (string/lower-case (first args))) [[(action-insert [:users user-id :content] false)]
                                                   (str user-id " has changed content setting to false.")]
        :else [[] "You must enter 'yes' or 'no'."])
      [[] "You must enter 'yes' or 'no'."])
    [[] "User is not registered."]))

;; Returns actions to set the option to see the image related to a news article
(defn set-image [users {:keys [args user-id]}]
  (if (contains? users user-id)
    (if (= 1 (count args))
      (cond
        (= "yes" (string/lower-case (first args))) [[(action-insert [:users user-id :image] true)]
                                                    (str user-id " has changed image setting to true.")]
        (= "no" (string/lower-case (first args))) [[(action-insert [:users user-id :image] false)]
                                                   (str user-id " has changed image setting to false.")]
        :else [[] "You must enter 'yes' or 'no'."])
      [[] "You must enter 'yes' or 'no'."])
    [[] "User is not registered."]))

;; My API key for https://newsapi.org/v2
(def apiKey "4ce6d2e75fe9467fbf5f7bba147274cd")

;; Pulls the top articles for a topic and returns a list of 10 articles
(defn get-top-articles [topic]
  (get (json/read-json
         (:body (client/get (str "https://newsapi.org/v2/top-headlines?pageSize=10&country=us&category=" topic
                                 "&apiKey=" apiKey)))) "articles"))

;; Pulls articles relevant to a parameter list of query words and returns
;; a list of 10 articles
(defn find-articles [query]
  (get (json/read-json
              (:body (client/get
                      (str "https://newsapi.org/v2/everything?pageSize=10&sources="
                           "the-wall-street-journal,the-new-york-times,cnn,msnbc"
                           "&q=" (string/join "%20" query)
                           "&apiKey=" apiKey)))) "articles"))

;; Returns actions to update the state with all the top articles for each of
;; the possible topics. Only my phone can call this function as a result of
;; a change in implementation plan. It would be difficult and confusing for me
;; to figure out how to make this function run on its own at a specific time of
;; day, so I decided to just text this command myself.
(defn update-all-top-articles [_ {:keys [_ user-id]}]
 (if (string/includes? user-id "9089388920")
  (let [actions (for [topic topics]
                  (action-insert [:articles topic] (get-top-articles topic)))]
    (conj [actions] "Daily update complete."))
  [[] "Invalid command."]))

;; Returns a list of articles where unecessary keys and values are removed
;; depending on the user's preferences given by the quantity, content, and
;; image parameters. Quantity is an int, content and image are booleans
(defn parse-articles [articles quantity content image]
  (for [n (range (min (count articles) quantity))]
    (let [article (dissoc (get articles n) "source" "author" "publishedAt")]
      (cond
        (and (not content) (not image)) (dissoc article "content" "urlToImage")
        (not content) (dissoc article "content")
        (not image) (dissoc article "urlToImage")
        :else article))))

;; Gets and processes articles for each user based on their subscriptions,
;; articles is a map with topics as key and lists of articles as values,
;; users is a map with user-id as key and each user's settings as values,
;; returns a list of actions to send texts to each user for each article
(defn article-actions-for-users [articles users]
  (let [actions (flatten (for [[user-id user] users]
                           (let [user-articles (flatten (for [topic (keys (:topics user))]
                                                          (parse-articles (get articles topic) (:quantity user)
                                                                          (:content user) (:image user))))]
                             (for [parsed-article user-articles]
                               (let [msg (for [[k v] parsed-article] (str k ": " v))]
                                 (action-send-msg user-id (string/join "\n" msg)))))))]
    (conj [actions] "Articles successfully sent.")))

;; Calls the article-actions-for-users function to get a list of actions to
;; take to send texts to users after checking that there are articles and
;; users stored in the state. In addition, only I can call this function.
(defn send-articles [state {:keys [_ user-id]}]
  (if (string/includes? user-id "9089388920")
    (let [articles (:articles state)
          users (:users state)]
      (cond
        (not articles) [[] (str state)]
        (not users) [[] "There are no registered users."]
        :else (article-actions-for-users articles users)))
    [[] "Invalid command."]))

;; Gets from the stored state if possible a list of articles related to an input
;; topic, parses them, and returns the actions necessary to send texts to the user
;; about each article
(defn show-news-for-topic [state {:keys [args user-id]}]
  (cond
    (not (:articles state)) [[] "No articles are stored."]
    (not (contains? (:users state) user-id)) [[] "User has not been registered."]
    (not= 1 (count args)) [[] "You must enter one topic."]
    (not (contains? topics (first args))) [[] "You must enter a valid topic."]
    :else (let [topic (first args)
                articles (get (:articles state) topic)
                user (get (:users state) user-id)
                parsed-articles (parse-articles articles (:quantity user) (:content user) (:image user))
                actions (for [parsed-article parsed-articles]
                          (let [msg (for [[k v] parsed-article] (str k ": " v))]
                            (action-send-msg user-id (string/join "\n" msg))))]
            (conj [actions] (str "News for topic: " topic ".")))))

;; Uses the find-articles function to pull article data searched using an input
;; list of words and returns the actions necessary to send texts to the user
;; about each article
(defn show-news-for-query [user {:keys [args user-id]}]
  (cond
    (or (not args) (not (first args))) [[] "You must enter a query."]
    (not user) [[] "User has not been registered."]
    :else (let [articles (find-articles args)
                parsed-articles (parse-articles articles (:quantity user) (:content user) (:image user))
                actions (for [parsed-article parsed-articles]
                          (let [msg (for [[k v] parsed-article] (str k ": " v))]
                            (action-send-msg user-id (string/join "\n" msg))))]
            (conj [actions] (str "News for query: " (string/join " " args) ".")))))

(def routes {"default"  (stateless (fn [& args] "Unknown command."))
             "welcome"  (stateless welcome)
             "homepage" (stateless homepage)
             "office"   (stateless office-hours)
             "topics"   (stateless show-topics)
             "expert"   add-expert
             "ask"      ask-experts
             "answer"   answer-question
             "register" add-user
             "name"     set-name
             "subscribe" subscribe
             "unsubscribe" unsubscribe
             "quantity" set-quantity
             "content"  set-content
             "image"    set-image
             "update"   update-all-top-articles
             "send"     send-articles
             "newstopic" show-news-for-topic
             "newsquery" show-news-for-query})

;; End functions for newstexter


;; Don't edit!
(defn handle-message
  "
    This function orchestrates the processing of incoming messages
    and glues all of the pieces of the processing pipeline together.

    The basic flow to handle a message is as follows:

    1. Create the router that will be used later to find the
       function to handle the message
    2. Parse the message
    3. Load any saved state that is going to be needed to process
       the message (e.g., querying the list of experts, etc.)
    4. Find the function that can handle the message
    5. Call the handler function with the state from #3 and
       the message
    6. Run the different actions that the handler returned...these actions
       will be bound to different implementations depending on the environemnt
       (e.g., in test, the actions aren't going to send real text messages)
    7. Return the string response to the message

  "
  [{:keys [state-mgr] :as system} src msg]
  (go
    (println "=========================================")
    (println "  Processing:\"" msg "\" from" src)
    (let [rtr    (create-router routes)
          _      (println "  Router:" rtr)
          pmsg   (assoc (parsed-msg msg) :user-id src)
          _      (println "  Parsed msg:" pmsg)
          state  (<! (read-state state-mgr pmsg))
          _      (println "  Read state:" state)
          hdlr   (rtr pmsg)
          _      (println "  Hdlr:" hdlr)
          [as o] (hdlr state pmsg)
          _      (println "  Hdlr result:" [as o])
          arslt  (<! (process-actions system as))
          _      (println "  Action results:" arslt)]
      (println "=========================================")
      o)))
