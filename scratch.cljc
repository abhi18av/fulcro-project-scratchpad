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


(declare remove-entity*)

(defn- cascade-delete*
  [state-map starting-entity cascade]
  (reduce
   (fn [s edge]
     (if (every? eql/ident? edge)
       (reduce (fn [s2 ident] (remove-entity* s2 ident cascade)) s edge)
       (remove-entity* s edge cascade)))
   state-map
   (set/intersection (set cascade) (set (keys starting-entity)))))


(>defn remove-entity*
       [state-map ident cascade]
       [map? eql/ident? (s/coll-of keyword? :kind set?) => map?]
       (let [tables                  (keep (fn [k]
                                             (let [candidate (get state-map k)]
                                               (when (and (map? candidate) (every? map? (vals candidate)))
                                                 k))) (keys state-map))
             remove-idents-at-path   (fn [state-map path]
                                       (let [v (clojure.core/get-in state-map path)]
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



;;;;;;;;;;;;;



(defn- cascade-delete*
  [state-map starting-entity cascade]
  (reduce
   (fn [s edge]
     (if (every? eql/ident? edge)
       (reduce (fn [s2 ident] (remove-entity* s2 ident cascade)) s edge)
       (remove-entity* s edge cascade)))
   state-map
   (set/intersection (set cascade) (set (keys starting-entity)))))



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
  (cascade-delete* state (get-in state [:person/id 1]) #{:person/cars}))




(assertions
 "Removes top-level to-one references"
 (-> (nsh/remove-entity* state [:car/id 1] #{}) :fastest-car nil?) => true
 "Removes top-level to-many refs"
 (-> (nsh/remove-entity* state [:person/id 1] #{}) :grandparents) => [[:person/id 2]]
 "Ignores denormalized data"
 (-> (nsh/remove-entity* state [:person/id 1] #{}) (get-in [:denorm :level-1 :level-2])) => denorm-data
 "Removes table-nested to-one references"
 (-> (nsh/remove-entity* state [:email/id 1] #{}) (get-in [:person/id 1 :person/email]) nil?) => true
 "Removes table-nested to-many refs"
 (-> (nsh/remove-entity* state [:car/id 1] #{}) (get-in [:person/id 1 :person/cars])) => [[:car/id 2]])
