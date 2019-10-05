(ns scratch
  "Functions that can be used against a normalized Fulcro state database."
  #?(:cljs (:require-macros com.fulcrologic.fulcro.algorithms.normalized-state-helpers))
  (:refer-clojure :exclude [get-in])
  (:require
    [clojure.walk :as walk]
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


;;============================================================================



(def denorm-data {:a [[:b 1]] :b [:b 1]})

(def app-db
  ;; These are unique for an individual
  {:email/id  {1 {:email/id  1
                  :email/url "1@test.com"}
               2 {:email/id  2
                  :email/url "2@test.com"}
               3 {:email/id  3
                  :email/url "3@test.com"}
               4 {:email/id  4
                  :email/url "4@test.com"}
               5 {:email/id  5
                  :email/url "5@test.com"}
               6 {:email/id  6
                  :email/url "6@test.com"}
               7 {:email/id  7
                  :email/url "7@test.com"}}
  ;; All people with relationship to other people as children and spouses
   :person/id {1 {:person/id       1
                  :person/name     "person-1"
                  :person/spouse   [:person/id 2]
                  :person/email    [:email/id 1]
                  :person/cars     [[:car/id 1]]
                  :person/children [:person/id 3
                                    :person/id 4
                                    :person/id 5]}
               2 {:person/id       2
                  :person/email    [:email/id 2]
                  :person/name     "person-2"
                  :person/spouse   [:person/id 2]
                  :person/cars     [[:car/id 2]]
                  :person/children [:person/id 3
                                    :person/id 4
                                    :person/id 5]}
               3 {:person/id       3
                  :person/email    [:email/id 3]
                  :person/name     "person-3"
                  :person/spouse   nil
                  :person/cars     nil
                  :person/children nil}
               4 {:person/id       4
                  :person/email    [:email/id 4]
                  :person/name     "person-4"
                  :person/spouse   [:person/id 6]
                  :person/cars     [[:car/id 4]]
                  :person/children [:person/id 8]}
               5 {:person/id       5
                  :person/email    [:email/id 5]
                  :person/name     "person-5"
                  :person/spouse   nil
                  :person/cars     [[:car/id 4]]
                  :person/children nil}
               6 {:person/id       6
                  :person/email    [:email/id 6]
                  :person/name     "person-6"
                  :person/spouse   [:person/id 4]
                  :person/cars     [[:car/id 1]
                                    [:car/id 2]]
                  :person/children [:person/id 7]}
               7 {:person/id       7
                  :person/email    [:email/id 7]
                  :person/name     "person-7"
                  :person/spouse   nil
                  :person/cars     [[:car/id 5]]
                  :person/children nil}
               8 {:person/id       8
                  :person/email    [:email/id 8]
                  :person/name     nil
                  :person/spouse   nil
                  :person/cars     nil
                  :person/children nil}}
;; All cars, the same car model could belong to multiple people
;; Different car models could have same engine
   :car/id    {1 {:car/id     1
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

   :engine/id {1 {:engine/id    1
                  :engine/model "engine-1"}

               2 {:engine/id    2
                  :engine/model "engine-2"}

               3 {:engine/id    3
                  :engine/model "engine-3"}

               4 {:engine/id    4
                  :engine/model "engine-4"}}

   :denorm    {:level-1 {:level-2 denorm-data}}

   :fastest-car [:car/id 3]

   :grandparents [[:person/id 1]
                  [:person/id 2]]

   :deceased  [[:person/id 8]]})


;;============================================================================


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
        (tree-path->db-path state path path))

       ([state path not-found]
        [map? vector? any? => vector?]
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
                  not-found)))))

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




(defn- purge-ident
  "Removes the dangling pointers to a `nil` caused by the removal of an ident
  at the toplevel. "
  [state-map [a-path a-value] ident]
  (let [vector-of-vectors? (if (and
                                (vector? a-value)
                                (every? vector? a-value))
                             true
                             false)]
    (cond
      ;; nil => dissoc from map
      (nil? a-value) (dissoc-in state-map a-path)
      ;; to-many => update the path
      vector-of-vectors? (assoc-in state-map a-path (apply vector (remove #{ident} a-value)))
      ;;to-one => remove from the state-map
      (vector? a-value) (if (= ident a-value)
                          (dissoc-in state-map a-path)))))




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

  (def denorm-data {:a [[:b 1]] :b [:b 1]})

  (def original-state
    {:a      [:b 1]
     :top-tm [[:d 1] [:b 1]]
     :denorm {:level-1 {:level-2 denorm-data}}
     :b      {1 {:c [:d 1]
                 :e [[:b 1] [:d 1]]}}
     :d      {1 {:value 42}}})


  (remove-entity* original-state [:d 1])

  (remove-entity* original-state [:d 1] #{:x})

  (remove-entity* original-state [:b 1])

  (remove-entity* original-state [:b 1] #{:x})


  (remove-entity* app-db [:person/id 1])


  (paths app-db)



  '())

;;============================================================================

(>defn remove-edge*
       ;; TODO
       ([state-map path-to-edge]
        [map? vector? => any?])


       ;; TODO needs cascade argument like remove-entity*
       ([state-map path-to-edge cascade]
        [map? vector? (s/coll-of keyword? :kind set?) => map?]))
