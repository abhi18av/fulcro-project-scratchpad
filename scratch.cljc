(ns scratch
  "Functions that can be used against a normalized Fulcro state database."
  #?(:cljs (:require-macros com.fulcrologic.fulcro.algorithms.normalized-state-helpers))
  (:refer-clojure :exclude [get-in])
  (:require
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
                               :person/children [:person/id 3
                                                 :person/id 4
                                                 :person/id 5]}
                            2 {:person/id       2
                               :person/email    [:email/id 2]
                               :person/name     "person-2"
                               :person/spouse   [:person/id 1]
                               :person/cars     [[:car/id 2]]
                               :person/accounts [[:account/id 3]]
                               :person/children [:person/id 3
                                                 :person/id 4
                                                 :person/id 5]}
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

(comment

  (tree-path->db-path app-db [:person/id 1 :person/cars 0 :car/engine :engine/model])

  (tree-path->db-path app-db [:person/id 6 :person/spouse])

  (tree-path->db-path app-db [:person/id 3 :person/email])

  (tree-path->db-path app-db [:person/id 9 :person/email])

  '())


;;============================================================================


(>defn get-in
       "Just like clojure.core/get-in, but if an element of the path is an ident it will follow the ident instead."
       ([state-map path]
        [map? vector? => any?]
        (get-in state-map path nil))

       ([state-map path not-found]
        [map? vector? any? => any?]
        (clojure.core/get-in state-map (tree-path->db-path state-map path) not-found)))

(comment
  (tree-path->db-path {:a [:b 1]
                       :b {1 {:c [:d 1]}}
                       :d {1 {:value 42}}}

                      [:a :c :value])

  (tree-path->db-path {:a {:c {:value 42}}}
                      [:a :c :value])

  (clojure.core/get-in
    {:a {:c {:value 42}}}
    [:a :c :value])

  (get-in
    {:a {:c {:value 42}}}
    [:a :c :value])

  (tree-path->db-path app-db [:person/id 1 :person/cars 0 :car/engine :engine/model])

  (get-in app-db [:person/id 1 :person/cars 0 :car/engine :engine/model])

  ;; if not found this will return nil
  (get-in app-db [:person/id 3 :person/cars 0 :car/engine :engine/model])

  '())








;;============================================================================

;;============================================================================

;; helper


(defn- paths
       "Walks the tree in a depth first manner and returns the possible paths"
       [m]
       (letfn [(paths* [ps ks m]
                       (reduce-kv
                         (fn [ps k v]
                             (if (map? v)
                               (paths* ps (conj ks k) v)
                               (conj ps (conj ks k))))
                         ps
                         m))]
              (paths* () [] m)))

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

(defn vector-of-vectors? [a-value]
      (if (and
            (vector? a-value)
            (every? vector? a-value))
        true
        false))

(defn nil-or-vector? [a-value] (or (nil? a-value))
      (vector? a-value))

(defn state-after-top-level-ident-dissoc [state-map ident]
      (dissoc-in state-map ident))

(defn all-paths-after-top-level-dissoc [state-map ident]
  (paths (state-after-top-level-ident-dissoc state-map ident)))


(all-paths-after-top-level-dissoc app-db [:person/id 1])


(defn all-values-at-path-after-top-level-dissoc [state-map ident]
  (let [value-at-path (fn [a-path]
                        (if (>= (count a-path) 4)
                          ;; don't follow idents for denormalized paths
                          (clojure.core/get-in (state-after-top-level-ident-dissoc state-map ident) a-path)
                          ;; follow idents for denormalized paths
                          (get-in (state-after-top-level-ident-dissoc state-map ident) a-path)))]
       (map (fn [a-path]
                (if (map? (value-at-path a-path))
                  ;; finds db-path from the original app-db
                  (tree-path->db-path app-db a-path)
                  (value-at-path a-path)))
            (all-paths-after-top-level-dissoc state-map ident))))

(all-values-at-path-after-top-level-dissoc app-db [:person/id 1])

(defn entity-path-value-map-after-top-level-dissoc [state-map ident]
  (zipmap (all-paths-after-top-level-dissoc state-map ident)
          (all-values-at-path-after-top-level-dissoc state-map ident)))

(entity-path-value-map-after-top-level-dissoc app-db [:person/id 1])


(defn purge-ident
       "Removes the dangling pointers to a `nil` caused by the removal of an ident
                      at the toplevel. "
       [state-map [a-path a-value] ident]
       (cond
              ;; nil => dissoc from map
              (nil? a-value) (dissoc-in state-map a-path)
              ;; to-many => update the path
              vector-of-vectors? (assoc-in state-map a-path (apply vector (remove #{ident} a-value)))
              ;;to-one => remove from the state-map
              (vector? a-value) (if (= ident a-value)
                                  (dissoc-in state-map a-path))))




(reduce #(purge-ident %1 %2 [:person/id])
        app-db
        (entity-path-value-map-after-dissoc-and-denorm app-db [:person/id 1]))





(>defn remove-entity*
       "Remove the given entity at the given ident. Also scans all tables and removes any to-one or to-many idents that are
        found that match `ident` (removes dangling pointers to the removed entity).

        The optional `cascade` parameter is a set of keywords that represent edges that should cause recursive deletes
        (i.e. it indicates edge names that *own* something, indicating it is safe to remove those entities as well).

        Returns the new state map with the entity(ies) removed."

       ([state-map ident]
        [map? eql/ident? => map?]
        (remove-entity* state-map ident #{}))


       ;;TODO implement the cascading feature
       ;; NOTE keywords in cascading should be uniquely owned


       ([state-map ident cascade]
        [map? eql/ident? (s/coll-of keyword? :kind set?) => map?]
        (let [ident ident
              cascade cascade
              nil-or-vector? (fn [x] (or (nil? x)
                                         (vector? x)))
              state-after-ident-dissoc (-> state-map (dissoc-in ident))
              all-paths-after-dissoc-and-denorm-keys (filter
                                                       (fn [a-path]
                                                           (if (< (count a-path) 4)
                                                             true
                                                             false))
                                                       (paths state-after-ident-dissoc))
              path-values-map (into {}
                                    (filter #(nil-or-vector? (second %))
                                            (zipmap
                                              all-paths-after-dissoc-and-denorm-keys
                                              (map #(get-in state-after-ident-dissoc %)
                                                   all-paths-after-dissoc-and-denorm-keys))))]
             (reduce #(purge-ident %1 %2 ident)
                     state-after-ident-dissoc
                     path-values-map))))

(comment

  (remove-entity* app-db [:person/id 1])

  (remove-entity* pruned-original-db [:person/id 1])

  ;;;;;;

  ;; NOTE path-db after dissoc and denorm


  ;;;;;;


  '())

;;============================================================================

(>defn remove-edge*
       ([state-map path-to-edge]
        [map? vector? => any?]
        (remove-edge* state-map path-to-edge #{}))

       ;; TODO needs cascade argument like remove-entity*


       ([state-map path-to-edge cascade]
        [map? vector? (s/coll-of keyword? :kind set?) => map?]))


;;;;;;;;;;;


(def all-paths-original-data
  (paths app-db))

(defn entity-path-value-map-original-data [a-path]
      (let [value-at-path (get-in app-db a-path)]
           (if (map? value-at-path)
             (tree-path->db-path app-db a-path)
             value-at-path)))


;; NOTE path-db original data


(def paths-db-original-data
  (zipmap
    all-paths-original-data
    (map #(entity-path-value-map-original-data %)
         all-paths-original-data)))

;; TODO a function to re-create the original db without the nil values


(defn prune-nils [app-db [a-path a-value]]
      (cond
        (nil? a-value) (dissoc-in app-db a-path)
        :else app-db))

(def nil-pruned-original-db
  (reduce prune-nils
          app-db
          paths-db-original-data))

(comment

  {:fastest-car [:car/id 1]
   :car/id      {1 {:car/engine [:engine/id 1]}}
   :engine/id   {1 {:engine/id    1
                    :engine/model "engine-1"}}}

  (remove-edge* state [:car/id 1 :car/engine])
  "Refuses to remove something that is not a normalized edge"
  (remove-edge* state [:engine/id 1 :engine/model])
  (remove-edge* state [:engine/id 1])
  (remove-edge* state [:engine/id])

  (let [state {:fastest-car  [:car/id 1]
               :car/id       {1 {:car/engine [:engine/id 1]}}
               :engine/id    {1 {:engine/id 1}}
               :engine/model "engine-1"}]
       (-> state
           (dissoc-in [:fastest-car])
           (get-in [:fastest-car])))

  (remove-entity* pruned-original-db [:person/id 1])

  (let [state {:a [:b 1]
               :b {1 {:c [:d 1]}}
               :d {1 {:value  42
                      :other  [:e 2]
                      :keeper [:e 3]}}
               :e {2 {:x 1}
                   3 {:y 1}}}]

       (assertions
         "Can cascade a delete across named edge, removing the ident on the :other edge from it's own table"
         (-> (nsh/remove-edge* state [:b 1 :c] #{:other})
             (get-in [:e 2])
             nil?) => true)

       (assertions
         "Can cascade a delete across named edges, leaving the ident on the :keeper edge it's in own table"
         (-> (nsh/remove-edge* state [:b 1 :c] #{:other})
             (get-in [:e 3])) => {:y 1}))

  '())



;;;;;;;;;;;;


{[:accounts/id 1 :account/id]    1,
 [:car/id 1 :car/model]          "car-1",
 [:accounts/id 7 :account/id]    7,
 [:person/id 6 :person/cars]     [[:car/id 1] [:car/id 2]],
 [:provider/id 1 :provider/id]   1,
 [:provider/id 4 :provider/name] "Yahoo",
 [:person/id 8 :person/spouse]   nil,
 [:accounts/id 4 :account/name]  "account-4",
 [:email/id 5 :email/url]        "5@test.com",
 [:email/id 3 :email/url]        "3@test.com",
 [:car/id 2 :car/engine]         [:engine/id 2],
 [:car/id 1 :car/engine]         [:engine/id 1],
 [:provider/id 4 :provider/id]   4,
 [:car/id 1 :car/id]             1,
 [:accounts/id 3 :account/name]  "account-3",
 [:provider/id 2 :provider/id]   2,
 [:car/id 2 :car/model]          "car-2",
 [:car/id 2 :car/id]             2,
 [:person/id 3 :person/id]       3,
 [:email/id 7 :email/url]        "7@test.com",
 [:provider/id 1 :provider/name] "Google",
 [:person/id 5 :person/accounts] [[:account/id 5]],
 [:email/id 4 :email/id]         4,
 [:person/id 8 :person/cars]     nil,
 [:email/id 6 :email/id]         6,
 [:email/id 7 :email/provider]   [:provider/id 2],
 [:provider/id 3 :provider/id]   3,
 [:engine/id 3 :engine/id]       3,
 [:accounts/id 4 :account/id]    4,
 [:car/id 4 :car/model]          "car-4",
 [:email/id 3 :email/provider]   [:provider/id 3],
 [:person/id 6 :person/email]    [:email/id 6],
 [:person/id 3 :person/cars]     nil,
 [:person/id 4 :person/spouse]   [:person/id 6],
 [:person/id 4 :person/email]    [:email/id 4],
 [:car/id 3 :car/model]          "car-3",
 [:person/id 7 :person/spouse]   nil,
 [:person/id 3 :person/spouse]   nil,
 [:email/id 2 :email/url]        "2@test.com",
 [:person/id 8 :person/name]     nil,
 [:engine/id 1 :engine/id]       1,
 [:person/id 4 :person/children] [:person/id 8],
 [:person/id 5 :person/cars]     [[:car/id 4]],
 [:person/id 6 :person/accounts] [[:account/id 6]],
 [:person/id 5 :person/name]     "person-5",
 [:accounts/id 1 :account/name]  "account-1",
 [:email/id 5 :email/provider]   [:provider/id 2],
 [:email/id 8 :email/id]         8,
 [:email/id 2 :email/provider]   [:provider/id 1],
 [:accounts/id 3 :account/id]    3,
 [:person/id 8 :person/accounts] nil,
 [:person/id 4 :person/id]       4,
 [:person/id 5 :person/children] nil,
 [:email/id 1 :email/url]        "1@test.com",
 [:person/id 6 :person/children] [:person/id 7],
 [:person/id 3 :person/email]    [:email/id 3],
 [:email/id 3 :email/id]         3,
 [:person/id 3 :person/children] nil,
 [:person/id 8 :person/children] nil,
 [:person/id 7 :person/children] nil,
 [:car/id 4 :car/engine]         [:engine/id 2],
 [:person/id 7 :person/id]       7,
 [:email/id 7 :email/id]         7,
 [:email/id 4 :email/provider]   [:provider/id 1],
 [:person/id 4 :person/name]     "person-4",
 [:email/id 8 :email/url]        "8@test.com",
 [:person/id 4 :person/accounts] [[:account/id 4]],
 [:accounts/id 2 :account/name]  "account-2",
 [:person/id 6 :person/name]     "person-6",
 [:accounts/id 5 :account/name]  "account-5",
 [:email/id 6 :email/url]        "6@test.com",
 [:car/id 3 :car/engine]         [:engine/id 3],
 [:car/id 3 :car/id]             3,
 [:email/id 4 :email/url]        "4@test.com",
 [:accounts/id 2 :account/id]    2,
 [:person/id 2 :person/children] [:person/id 3 :person/id 4 :person/id 5],
 [:person/id 3 :person/name]     "person-3",
 [:fastest-car]                  [:car/id 3],
 [:email/id 1 :email/id]         1,
 [:car/id 4 :car/id]             4,
 [:engine/id 2 :engine/id]       2,
 [:person/id 7 :person/email]    [:email/id 7],
 [:car/id 5 :car/id]             5,
 [:person/id 3 :person/accounts] nil,
 [:person/id 5 :person/spouse]   nil,
 [:engine/id 4 :engine/id]       4,
 [:person/id 6 :person/spouse]   [:person/id 4],
 [:person/id 2 :person/accounts] [[:account/id 3]],
 [:person/id 8 :person/email]    [:email/id 8],
 [:person/id 2 :person/cars]     [[:car/id 2]],
 [:person/id 6 :person/id]       6,
 [:provider/id 3 :provider/name] "Tencent",
 [:accounts/id 6 :account/id]    6,
 [:car/id 5 :car/model]          "car-5",
 [:person/id 2 :person/spouse]   nil,
 [:email/id 2 :email/id]         2,
 [:engine/id 1 :engine/model]    "engine-1",
 [:person/id 4 :person/cars]     [[:car/id 4]],
 [:person/id 2 :person/name]     "person-2",
 [:email/id 8 :email/provider]   [:provider/id 3],
 [:email/id 1 :email/provider]   [:provider/id 1],
 [:deceased]                     [[:person/id 8]],
 [:car/id 5 :car/engine]         [:engine/id 4],
 [:engine/id 2 :engine/model]    "engine-2",
 [:person/id 7 :person/name]     "person-7",
 [:accounts/id 6 :account/name]  "account-6",
 [:person/id 2 :person/id]       2,
 [:person/id 2 :person/email]    [:email/id 2],
 [:accounts/id 7 :account/name]  "account-7",
 [:person/id 5 :person/id]       5,
 [:accounts/id 5 :account/id]    5,
 [:person/id 8 :person/id]       8,
 [:engine/id 4 :engine/model]    "engine-4",
 [:provider/id 2 :provider/name] "Microsoft",
 [:engine/id 3 :engine/model]    "engine-3",
 [:email/id 5 :email/id]         5,
 [:grandparents]                 [[:person/id 1] [:person/id 2]],
 [:person/id 7 :person/accounts] [[:account/id 7]],
 [:email/id 6 :email/provider]   [:provider/id 3],
 [:person/id 5 :person/email]    [:email/id 5],
 [:person/id 7 :person/cars]     [[:car/id 5]]}
