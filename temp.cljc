(ns temp)


;;;;;;;;;;;;
;;;;;;;;;;;
;;;;;;;;;;;;

(>defn sort-idents-by
       "Intended to be used as
   ```
   (sort-idents-by :entity/field vector-of-idents)
   ```

   Can facilitate:
   ```
   (swap! state-map update-in [:entity 1 :list] #(sort-idents-by :list/field %))
   ```
  "
       [entity-field vector-of-idents]
       [keyword? vector? => any?]
       (mapv (fn [x] (first (into [] x)))
             (sort-by entity-field
                      (map (fn [[k v]] {k v}) vector-of-idents))))


;;;;;;;;;;;;
;;;;;;;;;;;
;;;;;;;;;;;;



(specification "sort-idents-by"
  (behavior "Given a vector of idents and sorting parameter"
    (let [state (atom {:person/id {1 {:person/name              "person-1"
                                      :person/age               90
                                      :person/random-collection [[:person/id 3]
                                                                 [:car/id 9]
                                                                 [:person/id 5]
                                                                 [:car/id 5]
                                                                 [:person/id 1]]

                                      :person/children          [[:person/id 3]
                                                                 [:person/id 9]
                                                                 [:person/id 5]
                                                                 [:person/id 1]]}}})]

      (assertions
        "Returns an ordered vector of idents"
        (nsh/sort-idents-by :person/id (get-in @state [:person/id 1 :person/children]))
        => [[:person/id 1] [:person/id 3] [:person/id 5] [:person/id 9]]

        "Ignores the idents different from the sorting parameter"
        (nsh/sort-idents-by :person/id (get-in @state [:person/id 1 :person/random-collection]))
        => [[:car/id 9] [:car/id 5] [:person/id 1] [:person/id 3] [:person/id 5]]))))
