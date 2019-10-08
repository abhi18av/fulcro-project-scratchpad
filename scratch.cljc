(ns scratch
  "Functions that can be used against a normalized Fulcro state database."
  #?(:cljs (:require-macros com.fulcrologic.fulcro.algorithms.normalized-state-helpers))
  (:refer-clojure :exclude [get-in])
  (:require
   [clojure.set :as set]
   [com.fulcrologic.fulcro.components :as comp :refer [defsc]]
   #?(:clj  [com.fulcrologic.fulcro.dom-server :as dom]
      :cljs [com.fulcrologic.fulcro.dom :as dom])
   [edn-query-language.core :as eql]
   [clojure.spec.alpha :as s]
   [ghostwheel.core :refer [>defn =>]]
   [com.fulcrologic.fulcro.algorithms.data-targeting :as targeting]
   [com.fulcrologic.fulcro.algorithms.denormalize :as fdn]
   [com.fulcrologic.fulcro.algorithms.merge :as merge]
   [com.fulcrologic.fulcro.components :as comp]))

(def app-db {:denorm       {:level-1 {:level-2 {:a [[:b 1]] :b [:b 1]}}}
             ;; All people with relationship to other people as children and spouses
             :person/id    {1 {:person/id       1
                               :person/name     "person-1"
                               :person/spouse   [:person/id 2]
                               :person/email    [:email/id 1]
                               :person/cars     [[:car/id 1]]
                               :person/accounts [[:account/id 1]
                                                 [:account/id 2]]
                               :person/children [[:person/id 3]
                                                 [:person/id 4]
                                                 [:person/id 5]]}
                            2 {:person/id       2
                               :person/email    [:email/id 2]
                               :person/name     "person-2"
                               :person/spouse   [:person/id 1]
                               :person/cars     [[:car/id 2]]
                               :person/accounts [[:account/id 3]]
                               :person/children [[:person/id 3]
                                                 [:person/id 4]
                                                 [:person/id 5]]}
                            3 {:person/id       3
                               :person/email    [:email/id 3]
                               :person/name     "person-3"
                               :person/spouse   nil
                               :person/accounts nil
                               :person/cars     nil
                               :person/children nil}
                            4 {:person/id       4
                               :person/email    [:email/id 4]
                               :person/name     "person-4"
                               :person/spouse   [:person/id 6]
                               :person/accounts [[:account/id 4]]
                               :person/cars     [[:car/id 4]]
                               :person/children [:person/id 8]}
                            5 {:person/id       5
                               :person/email    [:email/id 5]
                               :person/name     "person-5"
                               :person/spouse   nil
                               :person/accounts [[:account/id 5]]
                               :person/cars     [[:car/id 4]]
                               :person/children nil}
                            6 {:person/id       6
                               :person/email    [:email/id 6]
                               :person/name     "person-6"
                               :person/spouse   [:person/id 4]
                               :person/accounts [[:account/id 6]]
                               :person/cars     [[:car/id 1]
                                                 [:car/id 2]]
                               :person/children [:person/id 7]}
                            7 {:person/id       7
                               :person/email    [:email/id 7]
                               :person/name     "person-7"
                               :person/spouse   nil
                               :person/accounts [[:account/id 7]]
                               :person/cars     [[:car/id 5]]
                               :person/children nil}
                            8 {:person/id       8
                               :person/email    [:email/id 8]
                               :person/name     nil
                               :person/accounts nil
                               :person/spouse   nil
                               :person/cars     nil
                               :person/children nil}}
             ;; These are unique for an individual
             :email/id     {1 {:email/id       1
                               :email/url      "1@test.com"
                               :email/provider [:provider/id 1]}
                            2 {:email/id       2
                               :email/url      "2@test.com"
                               :email/provider [:provider/id 1]}
                            3 {:email/id       3
                               :email/url      "3@test.com"
                               :email/provider [:provider/id 3]}
                            4 {:email/id       4
                               :email/url      "4@test.com"
                               :email/provider [:provider/id 1]}
                            5 {:email/id       5
                               :email/url      "5@test.com"
                               :email/provider [:provider/id 2]}
                            6 {:email/id       6
                               :email/url      "6@test.com"
                               :email/provider [:provider/id 3]}
                            7 {:email/id       7
                               :email/url      "7@test.com"
                               :email/provider [:provider/id 2]}
                            8 {:email/id       8
                               :email/url      "8@test.com"
                               :email/provider [:provider/id 3]}}
             :provider/id  {1 {:provider/id   1
                               :provider/name "Google"}
                            2 {:provider/id   2
                               :provider/name "Microsoft"}
                            3 {:provider/id   3
                               :provider/name "Tencent"}
                            4 {:provider/id   4
                               :provider/name "Yahoo"}}
             ;; Bank accounts which are removed when a person is removed and are uniquely owned
             :accounts/id  {1 {:account/id   1
                               :account/name "account-1"}
                            2 {:account/id   2
                               :account/name "account-2"}
                            3 {:account/id   3
                               :account/name "account-3"}
                            4 {:account/id   4
                               :account/name "account-4"}
                            5 {:account/id   5
                               :account/name "account-5"}
                            6 {:account/id   6
                               :account/name "account-6"}
                            7 {:account/id   7
                               :account/name "account-7"}}
             ;; All cars, the same car model could belong to multiple people
             ;; Different car models could have same engine
             :car/id       {1 {:car/id     1
                               :car/model  "car-1"
                               :car/engine [:engine/id 1]}
                            2 {:car/id     2
                               :car/model  "car-2"
                               :car/engine [:engine/id 2]}
                            3 {:car/id     3
                               :car/model  "car-3"
                               :car/engine [:engine/id 3]}
                            4 {:car/id     4
                               :car/model  "car-4"
                               :car/engine [:engine/id 2]}
                            5 {:car/id     5
                               :car/model  "car-5"
                               :car/engine [:engine/id 4]}}
             :engine/id    {1 {:engine/id    1
                               :engine/model "engine-1"}
                            2 {:engine/id    2
                               :engine/model "engine-2"}
                            3 {:engine/id    3
                               :engine/model "engine-3"}
                            4 {:engine/id    4
                               :engine/model "engine-4"}}
             :fastest-car  [:car/id 3]
             :grandparents [[:person/id 1]
                            [:person/id 2]]
             :deceased     [[:person/id 8]]})

;;============================================================================

;; if the tree path doesn't exist in the normalized data then return nil/path ???


(>defn tree-path->db-path
       "Convert a 'denormalized' path into a normalized one by walking the path in state and honoring ident-based edges.

  For example, one might find this to be true for a normalized db:

  ```
  state => {:person/id {1 {:person/id 1 :person/spouse [:person/id 3]}
                        3 {:person/id 3 :person/first-name ...}}}

  (tree-path->db-path state [:person/id 1 :person/spouse :person/first-name])
  => [:person/id 3 :person/first-name]
  ```
  "
       ([state path]
        [map? vector? => vector?]
        (loop [[h & t] path
               new-path []]
          (if h
            (let [np (conj new-path h)
                  c (clojure.core/get-in state np)]
              (if (eql/ident? c)
                (recur t c)
                (recur t (conj new-path h))))
            (if (not= path new-path)
              new-path
              path)))))



;;============================================================================


(>defn get-in
       "Just like clojure.core/get-in, but if an element of the path is an ident it will follow the ident instead."
       ([state-map path]
        [map? vector? => any?]
        (get-in state-map path nil))
       ([state-map path not-found]
        [map? vector? any? => any?]
        (clojure.core/get-in state-map (tree-path->db-path state-map path) not-found)))


;;============================================================================

;; helper


(defn- dissoc-in
  "Dissociates an entry from a nested associative structure returning a new
                  nested structure. keys is a sequence of keys. Any empty maps that result
                  will not be present in the new structure."
  [m [k & ks :as keys]]
  (if ks
    (if-let [nextmap (get m k)]
      (let [newmap (dissoc-in nextmap ks)]
        (if (seq newmap)
          (assoc m k newmap)
          (dissoc m k)))
      m)
    (dissoc m k)))


;;============================================================================


(declare remove-entity-tk*)

(defn- cascade-delete*
  [state-map starting-entity cascade]
  (reduce
   (fn [s edge]
     (if (every? eql/ident? edge)
       (reduce (fn [s2 ident] (remove-entity-tk* s2 ident cascade)) s edge)
       (remove-entity-tk* s edge cascade)))
   state-map
   (set/intersection (set cascade) (set (keys starting-entity)))))

(>defn remove-entity-tk*
       [state-map ident cascade]
       [map? eql/ident? (s/coll-of keyword? :kind set?) => map?]
       (let [tables                  (keep (fn [k]
                                             (let [candidate (get state-map k)]
                                               (when (and (map? candidate) (every? map? (vals candidate)))
                                                 k))) (keys state-map))
             remove-idents-at-path   (fn [state-map path]
                                       (let [v (get-in state-map path)]
                                         (cond
                                           (= v ident) (dissoc-in state-map path)
                                           (every? eql/ident? v) (merge/remove-ident* state-map ident path)
                                           :else state-map)))
             candidate-paths         (fn [state-map top-key]     ; allow top-key to be nil to "mean" root node
                                       (map (fn [k]
                                              (if top-key
                                                [top-key k]
                                                [k]))
                                            (keys (get state-map top-key))))
             remove-ident-from-table (fn [state-map table]
                                       (reduce
                                        remove-idents-at-path
                                        state-map
                                        (candidate-paths state-map table)))
             state-without-entity    (->
                                  ;; remove the pointers to the entity
                                      (reduce remove-ident-from-table state-map tables)
                                  ;; remove the top-level edges that point to the entity
                                      (remove-ident-from-table nil)
                                  ;; remove the entity
                                      (dissoc-in ident))
             target-entity           (get-in state-map ident)
             final-state             (cascade-delete* state-without-entity target-entity cascade)]
         final-state))

(remove-entity-tk* app-db [:person/id 1] #{})

(def state-map {:fastest-car  [:car/id 1]
                :grandparents [[:person/id 1] [:person/id 2]]
                :denorm       {:level-1 {:level-2 {:a [[:person/id 1] [:person/id 2]] :b [:person/id 1]}}}
                :person/id    {1 {:person/name     "person-1"
                                  :person/spouse   [:person/id 2]
                                  :person/email    [:email/id 1]
                                  :person/cars     [[:car/id 1]]
                                  :person/children [[:person/id 3]
                                                    [:person/id 4]
                                                    [:person/id 5]]}
                               2 {:person/name     "person-2"
                                  :person/spouse   [:person/id 1]
                                  :person/children [[:person/id 3]
                                                    [:person/id 4]
                                                    [:person/id 5]]}
                               3 {:person/name "person-3"}
                               4 {:person/name "person-4"}
                               5 {:person/name "person-5"}}
                :car/id       {1 {:car/model  "model-1"
                                  :car/engine [:engine/id 1]}}
                :engine/id    {1 {:engine/name "engine-1"}
                               2 {:engine/name "engine-2"}}
                :email/id     {1 {:email/provider "Google"}
                               2 {:email/provider "Microsoft"}}})

(keep (fn [k]
        (let [candidate (get state-map k)]
          (when (and (map? candidate)
                     (every? map? (vals candidate)))
            k)))
      (keys state-map))

(keys state-map)

;;;;;;;;;;;;;





(specification "remove-entity*"
  (behavior "Without cascading"
    (let [denorm-data {:a [[:person/id 1] [:person/id 2]] :b [:person/id 1]}

          state {:fastest-car  [:car/id 1]
                 :grandparents [[:person/id 1] [:person/id 2]]
                 :denorm       {:level-1 {:level-2 denorm-data}}
                 :person/id    {1 {:person/name  "person-1"
                                   :person/cars  [[:car/id 1] [:car/id 2]]
                                   :person/email [:email/id 1]}
                                2 {:person/name  "person-2"
                                   :person/cars  [[:car/id 1]]
                                   :person/email [:email/id 2]}}
                 :car/id       {1 {:car/model "model-1"}
                                2 {:car/model "model-2"}}
                 :email/id     {1 {:email/provider "Google"}
                                2 {:email/provider "Microsoft"}}}]
      (assertions
        "Removes the entity itself from the database"
        (-> (nsh/remove-entity* state [:person/id 1] #{})
            (get-in [:person/id 1])
            nil?) => true
        "Removes top-level to-one references"
        (-> (nsh/remove-entity* state [:car/id 1] #{}) :fastest-car nil?) => true
        "Removes top-level to-many refs"
        (-> (nsh/remove-entity* state [:person/id 1] #{}) :grandparents) => [[:person/id 2]]
        "Ignores denormalized data"
        (-> (nsh/remove-entity* state [:person/id 1] #{}) (get-in [:denorm :level-1 :level-2])) => denorm-data
        "Removes table-nested to-one references"
        (-> (nsh/remove-entity* state [:email/id 1] #{}) (get-in [:person/id 1 :person/email]) nil?) => true
        "Removes table-nested to-many refs"
        (-> (nsh/remove-entity* state [:car/id 1] #{}) (get-in [:person/id 1 :person/cars])) => [[:car/id 2]])))



  (behavior "With cascading, non-recursive behavior"
    (let [state {:person/id {1 {:person/name     "person-1"
                                :person/spouse   [:person/id 2]
                                :person/email    [:email/id 1]
                                :person/cars     [[:car/id 1]]
                                :person/children [[:person/id 3]]}
                             2 {:person/name     "person-2"
                                :person/spouse   [:person/id 1]
                                :person/children [[:person/id 3]]}
                             3 {:person/name "person-3"}}
                 :car/id    {1 {:car/model "model-1"}}
                 :engine/id {1 {:engine/name "engine-1"}}
                 :email/id  {1 {:email/provider "Google"}}}]
      (assertions

        "Removes a single, to-one and non-recursive cascased entity"
        (-> (nsh/remove-entity* state [:person/id 1] #{:person/email})
            (get [:email/id 1])
            nil?) => true

        "Removes a single, to-many and non-recursive cascased entity"
        (-> (nsh/remove-entity* state [:person/id 1] #{:person/cars})
            (affects...
              (get [:car/id 1])
              (get [:car/id 2]))
            nil?) => true

        "Removes multiple, to-one and non-recursive cascased entity"
        (-> (nsh/remove-entity* state [:person/id 1] #{:person/email :person/cars})
            (affects...
              (get [:email/id 1])
              (get [:car/id 1])
              (get [:car/id 2]))
            nil?) => true)))




  (behavior "With cascading, recursive behavior"
    (let [state {:person/id    {1 {:person/name     "person-1"
                                   :person/spouse   [:person/id 2]
                                   :person/children [[:person/id 3]]}
                                2 {:person/name     "person-2"
                                   :person/spouse   [:person/id 1]
                                   :person/children [[:person/id 3]]}
                                3 {:person/name "person-3"}}}]
      (assertions

        "Removes a single, to-many cascased entities"
        (-> (nsh/remove-entity* state [:person/id 1] #{:person/children})
            (affects...
              (get [:person/id 3]))
            nil?) => true

        "Removes multiple, to-many cascased entities"
        (-> (nsh/remove-entity* state [:person/id 1] #{:person/children :person/spouse})
            (affects...
              (get [:person/id 2])
              (get [:person/id 3]))
            nil?) => true))))
