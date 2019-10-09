(ns temp)



(defn- normalized-paths
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
    (filter #(< (count %) 4)
            (paths* () [] m))))


(declare remove-entity*)

(defn- cascade-delete*
  [state-map starting-entity cascade]
  (reduce
    (fn [s edge]
      (if (every? eql/ident? edge)
        (reduce (fn [s2 ident] (remove-entity* s2 ident cascade)) s edge)
        (remove-entity* s edge cascade)))
    state-map
    ;; FIXME this must return an ident or vec of idents
    (set/intersection (set cascade) (set (keys starting-entity)))))

(>defn remove-entity*
  [state-map ident cascade]
  [map? eql/ident? (s/coll-of keyword? :kind set?) => map?]
  (let [
        tables (keep (fn [k]
                       (let [candidate (get state-map k)]
                         (when (and (map? candidate) (every? map? (vals candidate)))
                           k))) (keys state-map))

        non-tables (keep (fn [k]
                           (let [candidate (get state-map k)]
                             (when (vector? candidate)
                               [k])))
                         (keys state-map))
        remove-idents-at-path (fn [state-map path]
                                (let [v (clojure.core/get-in state-map path)]
                                  (cond
                                    (= v ident) (dissoc-in state-map path)
                                    (every? eql/ident? v) (merge/remove-ident* state-map ident path)
                                    :else state-map)))
                                  
        candidate-paths (fn [state-map table-name]
                           (filter (fn [a-path]
                                     (= table-name (first a-path)))
                                   (normalized-paths state-map)))

        remove-ident-from-table (fn [state-map table]
                                  (reduce
                                    remove-idents-at-path
                                    state-map
                                    (concat (candidate-paths state-map :person/id) non-tables)))

        state-without-entity (->
                               ;; remove the (non) table-nested pointers to the entity
                               (reduce remove-ident-from-table
                                       state-map
                                       tables)
                               ;; remove the top-level entity
                               (dissoc-in ident))
        target-entity (get-in state-map ident)
        final-state (cascade-delete* state-without-entity target-entity cascade)]
    final-state))

