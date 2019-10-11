(ns temp)


;;;;;;;;;;;;
;;;;;;;;;;;
;;;;;;;;;;;;





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
                                    (int? v) state1
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

        target-entity (get-in state1 ident)

        cascaded-idents (fn [original-state target-entity cascade]
                          (map
                            (fn [x] (clojure.core/get-in original-state
                                                         (conj [(first ident) ((first ident) target-entity)] x)))
                            (set/intersection
                              cascade
                              (set (keys target-entity)))))

        final-state (reduce
                      (fn [s edge]
                        (if (every? eql/ident? edge)
                          (reduce (fn [s2 ident] (remove-entity* s2 ident cascade)) s edge)
                          (remove-entity* s edge cascade)))
                      state-without-entity
                      (cascaded-idents state1
                                       target-entity
                                       cascade))]

    final-state))

;;;;;;;;;;;;
;;;;;;;;;;;
;;;;;;;;;;;;



(specification "remove-entity*"
  (behavior "Without cascading"
    (let [denorm-data {:a [[:person/id 1] [:person/id 2]]
                       :b [:person/id 1]}

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
            (get-in [:person/id 1]
             => nil))
        "Removes top-level to-one references"
        (-> (nsh/remove-entity* state [:car/id 1] #{}) :fastest-car) => []
        "Removes top-level to-many refs"
        (-> (nsh/remove-entity* state [:person/id 1] #{}) :grandparents) => [[:person/id 2]]
        "Ignores denormalized data"
        (-> (nsh/remove-entity* state [:person/id 1] #{}) (get-in [:denorm :level-1 :level-2])) => denorm-data
        "Removes table-nested to-one references"
        (-> (nsh/remove-entity* state [:email/id 1] #{}) (get-in [:person/id 1 :person/email]) nil?) => true
        "Removes table-nested to-many refs"
        (-> (nsh/remove-entity* state [:car/id 1] #{}) (get-in [:person/id 1 :person/cars])) => [[:car/id 2]])))



  (behavior "With cascading, non-recursive behavior"
    (let [state {:fastest-car  [:car/id 1]
                 :grandparents [[:person/id 1] [:person/id 2]]
                 :denorm       {:level-1 {:level-2 {:a [[:person/id 1] [:person/id 2]]
                                                    :b [:person/id 1]}}}
                 :person/id    {1 {:person/id       1
                                   :person/spouse   [:person/id 2]
                                   :person/email    [:email/id 1]
                                   :person/cars     [[:car/id 1]
                                                     [:car/id 2]]
                                   :person/children [[:person/id 3]]}
                                2 {:person/id       2
                                   :person/spouse   [:person/id 1]
                                   :person/children [[:person/id 3]]}
                                3 {:person/id 3}}
                 :car/id       {1 {:car/id    1
                                   :car/model "model-1"}
                                2 {:car/id    2
                                   :car/model "model-2"}}
                 :engine/id    {1 {:engine/id   1
                                   :engine/name "engine-1"}}
                 :email/id     {1 {:email/id       1
                                   :email/provider "Google"}}}]
      (assertions

        "Removes a single, to-one and non-recursive cascased entity"
        (-> (nsh/remove-entity* state [:person/id 1] #{:person/email})
            (get [:email/id 1])
            nil?) => true

        "Removes a single, to-many and non-recursive cascased entity"
        (let [new-state (nsh/remove-entity* state [:person/id 1] #{:person/cars})]
          (and
            (get new-state [:car/id 1])
            (get new-state [:car/id 2]))) => nil

        "Removes multiple, to-one and non-recursive cascased entity"
        (let [new-state (nsh/remove-entity* state [:person/id 1]
                                            #{:person/email :person/cars})]
          (and
            (get new-state [:email/id 1])
            (get new-state [:car/id 1])
            (get new-state [:car/id 2]))) => nil)))

  (behavior "With cascading, recursive behavior"
    (let [state {:fastest-car  [:car/id 1]
                 :grandparents [[:person/id 1] [:person/id 2]]
                 :denorm       {:level-1 {:level-2 {:a [[:person/id 1] [:person/id 2]]
                                                    :b [:person/id 1]}}}
                 :person/id    {1 {:person/id       1
                                   :person/spouse   [:person/id 2]
                                   :person/email    [:email/id 1]
                                   :person/cars     [[:car/id 1]
                                                     [:car/id 2]]
                                   :person/children [[:person/id 3]]}
                                2 {:person/id       2
                                   :person/spouse   [:person/id 1]
                                   :person/children [[:person/id 3]]}
                                3 {:person/id 3}}
                 :car/id       {1 {:car/id    1
                                   :car/model "model-1"}
                                2 {:car/id    2
                                   :car/model "model-2"}}
                 :engine/id    {1 {:engine/id   1
                                   :engine/name "engine-1"}}
                 :email/id     {1 {:email/id       1
                                   :email/provider "Google"}}}]
      (assertions

        "Removes a single, to-many cascased entities"
        (let [new-state (nsh/remove-entity* state [:person/id 1] #{:person/children})]
          (get new-state [:person/id 3])) => nil

        "Removes multiple, to-many cascased entities"
        (let [new-state (nsh/remove-entity* state [:person/id 1]
                                            #{:person/children :person/spouse})]
          (and
            (get new-state [:person/id 2])
            (get new-state [:person/id 3]))) => nil))))
