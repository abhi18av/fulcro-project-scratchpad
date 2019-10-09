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
       ([state1 path]
        [map? vector? => any?]
        (get-in state1 path nil))
       ([state1 path not-found]
        [map? vector? any? => any?]
        (clojure.core/get-in state1 (tree-path->db-path state1 path) not-found)))

;;============================================================================


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

(defn- normalized-paths
  "Walks the tree in a depth first manner and returns the normalized possible paths"
  [m]
  (letfn [(paths* [ps ks m]
            (reduce-kv
             (fn [ps k v]
               (if (map? v)
                 (paths* ps (conj ks k) v)
                 (conj ps (conj ks k))))
             ps
             m))]
    (filter #(< (count %) 4)
            (paths* () [] m))))

(declare remove-entity*)

(defn- cascade-delete*
  [state1 starting-entity cascade]
  (reduce
   (fn [s edge]
     (if (every? eql/ident? edge)
       (reduce (fn [s2 ident] (remove-entity* s2 ident cascade)) s edge)
       (remove-entity* s edge cascade)))
   state1
    ;; FIXME this must return an ident or vec of idents
   (set/intersection (set cascade) (set (keys starting-entity)))))

(>defn remove-entity*
       [state1 ident cascade]
       [map? eql/ident? (s/coll-of keyword? :kind set?) => map?]
       (let [tables (keep (fn [k]
                            (let [candidate (get state1 k)]
                              (when (and (map? candidate) (every? map? (vals candidate)))
                                k))) (keys state1))

             non-tables (keep (fn [k]
                                (let [candidate (get state1 k)]
                                  (when (vector? candidate)
                                    [k])))
                              (keys state1))
             remove-idents-at-path (fn [state1 path]
                                     (let [v (clojure.core/get-in state1 path)]
                                       (cond
                                         (= v ident) (dissoc-in state1 path)
                                         (every? eql/ident? v) (merge/remove-ident* state1 ident path)
                                         :else state1)))

             candidate-paths (fn [state1 table-name]
                               (filter (fn [a-path]
                                         (= table-name (first a-path)))
                                       (normalized-paths state1)))

             remove-ident-from-table (fn [state1 table]
                                       (reduce
                                        remove-idents-at-path
                                        state1
                                        (concat (candidate-paths state1 :person/id) non-tables)))

             state-without-entity (->
                               ;; remove the (non) table-nested pointers to the entity
                                   (reduce remove-ident-from-table
                                           state1
                                           tables)
                               ;; remove the top-level entity
                                   (dissoc-in ident))
             target-entity (clojure.core/get-in state1 ident)
             final-state (cascade-delete* state-without-entity target-entity cascade)]
         final-state))


;;;;;;;;;;;;;


(def state1 {:fastest-car  [:car/id 1]
            :grandparents [[:person/id 1] [:person/id 2]]
            :denorm       {:level-1 {:level-2 {:a [[:person/id 1] [:person/id 2]]
                                               :b [:person/id 1]}}}
            :person/id    {1 {:person/name  "person-1"
                              :person/cars  [[:car/id 1] [:car/id 2]]
                              :person/email [:email/id 1]}
                           2 {:person/name  "person-2"
                              :person/cars  [[:car/id 1]]
                              :person/email [:email/id 2]}}
            :car/id       {1 {:car/model "model-1"}
                           2 {:car/model "model-2"}}
            :email/id     {1 {:email/provider "Google"}
                           2 {:email/provider "Microsoft"}}})

;; behavior-1
(remove-entity* state1 [:person/id 1] #{})

(remove-entity* state1 [:car/id 1] #{})

(remove-entity* state1 [:person/id 1] #{})

(remove-entity* state1 [:person/id 1] #{})

(remove-entity* state1 [:email/id 1] #{})

(remove-entity* state1 [:car/id 1] #{})


;; behavior-2


(def state2 {:person/id {1 {:person/name     "person-1"}
                          :person/spouse   [:person/id 2]
                          :person/email    [:email/id 1]
                          :person/cars     [[:car/id 1]
                                            [:car/id 2]]
                          :person/children [[:person/id 3]]
                          2 {:person/name     "person-2"
                             :person/spouse   [:person/id 1]
                             :person/children [[:person/id 3]]}
                          3 {:person/name "person-3"}}
             :car/id    {1 {:car/model "model-1"}
                         2 {:car/model "model-2"}}
             :engine/id {1 {:engine/name "engine-1"}}
             :email/id  {1 {:email/provider "Google"}}})




;;"Removes a single, to-one and non-recursive cascased entity"
(remove-entity* state2 [:person/id 1] #{:person/email})

;;"Removes a single, to-many and non-recursive cascased entity"
(remove-entity* state2 [:person/id 1] #{:person/cars})

;;"Removes multiple, to-one and non-recursive cascased entity"
(remove-entity* state2 [:person/id 1] #{:person/email :person/cars})



