(ns scratch2)

user=> (sort-by count ["aaa" "bb" "c"])
("c" "bb" "aaa")

user=> (sort-by first [[1 2] [2 2] [2 3]])
([1 2] [2 2] [2 3])

user=> (sort-by first > [[1 2] [2 2] [2 3]])
([2 2] [2 3] [1 2])

user=> (sort-by :rank [{:rank 2} {:rank 3} {:rank 1}])
({:rank 1} {:rank 2} {:rank 3})






(def x [[:foo 2 :bar 11]
        [:bar 99 :foo 1]
        [:bar 55 :foo 2]
        [:foo 1 :bar 77]])

(sort-by :foo  x)


;sort by :foo, and where :foo is equal, sort by :bar
(sort-by (juxt :foo :bar) x)



(sort-by last {:b 1 :c 3 :a 2})


(sort-by last [[:a 3] [:b 2] [:c 3]])

(range 10)

(def vector-of-idents
   [[:entity/field 0]
    [:entity/field 7]
    [:entity/field 1]
    [:entity/field 2]
    [:entity/field 5]
    [:entity/field 6]
    [:entity/field 4]
    [:entity/field 5]
    [:entity/field 6]
    [:entity/field 4]
    [:entity/field 3]
    [:entity/field 5]
    [:entity/field 8]])

(sort-by last vector-of-idents)


(sort-by :entity/field vector-of-idents)


(sort-by (fn [x] (second x))
         vector-of-idents)


(sort-by second vector-of-idents)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;; TODO write tests
;; TODO
;;    (swap! state update-in [:entity 1 :list] sort-idents-by :list/field)
(defn sort-idents-by
  " Intended to be used as


   ```
   (sort-idents-by :entity/field vector-of-idents)
   ```
  "
  [entity-field vector-of-idents]
  (sort-by second vector-of-idents))

(comment
  ;; TODO


  (def state (atom {:grandparents [[:person/id 3] [:person/id 2]]
                    :person/id    {1 {:person/name     "person-1"
                                      :person/children [[:person/id 3]
                                                        [:person/id 9]
                                                        [:person/id 5]]}
                                   2 {:person/name "person-2"
                                      :person/cars [[:car/id 1]
                                                    [:car/id 2]]}}
                    :car/id       {1 {:car/model "model-1"}
                                   2 {:car/model "model-2"}}}))


  (sort-by second (clojure.core/get-in @state [:person/id 1 :person/children]))

  (swap! state update-in [:person/id 1 :person/children] sort-idents-by :person/id)

  '())




































































































