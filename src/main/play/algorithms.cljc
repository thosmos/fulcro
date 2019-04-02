(ns play.algorithms
  (:require
    [clojure.pprint]))

(defn lookup-ref? [v]
  (and (vector? v) (= 2 (count v)) (not (lookup-ref? (first v)))))

(defmacro spy [v]
  (let [expr (str v)]
    `(let [result# ~v]
       (println (str ~expr "\n => " (with-out-str (clojure.pprint/pprint result#))))
       result#)))

(defn iget
  "Like `get`, but follows idents"
  [entity k state-map]
  (let [v (get entity k)]
    (if (lookup-ref? v)
      (get-in state-map v)
      v)))

(declare denormalize)

(defn- add-props!
  "Walk the given AST children (which MUST be prop nodes), and add their values from `current-entity`
  (if found)."
  [transient-node entity ast-prop-children]
  (reduce
    (fn [n {:keys [key]}]
      (if-let [v (get entity key)]
        (assoc! n key v)
        n))
    transient-node
    ast-prop-children))

(defn add-join! [n {:keys [query key] :as join-node} entity state-map parent-node idents-seen]
  (let [v           (get entity key)
        is-ref?     (lookup-ref? v)
        loop?       (and is-ref? (contains? (get idents-seen key) v))
        recursive?  (= '... query)
        join-node   (if recursive? parent-node join-node)
        idents-seen (if is-ref?
                      (update idents-seen key (fnil conj #{}) v)
                      idents-seen)
        join-entity (if is-ref? (get-in state-map v) v)
        to-many?    (and (vector? join-entity) (lookup-ref? (first join-entity)))]
    (cond
      loop? (assoc! n key v)
      to-many? (assoc! n key
                 (into []
                   (keep (fn [lookup-ref]
                           (when-let [e (get-in state-map lookup-ref)]
                             (denormalize join-node e state-map idents-seen))))
                   join-entity))
      (and recursive? join-entity) (assoc! n key (denormalize parent-node join-entity state-map idents-seen))
      (map? join-entity) (assoc! n key (denormalize join-node join-entity state-map idents-seen))
      :otherwise n)))

(defn add-joins! [transient-node entity state-map parent-node ast-join-nodes idents-seen]
  (reduce
    (fn [n join-node] (add-join! n join-node entity state-map parent-node idents-seen))
    transient-node
    ast-join-nodes))

(def ^:dynamic *denormalize-time* 0)

(defn denormalize
  [{:keys [type children key] :as top-node} current-entity state-map idents-seen]
  (assert (not= type :prop))
  (let [grouped-children (group-by :type children)
        result-node      (transient {})
        result-node      (add-props! result-node current-entity (:prop grouped-children))
        result-node      (add-joins! result-node current-entity state-map
                           top-node
                           (:join grouped-children)
                           idents-seen)]
    (with-meta (persistent! result-node) {:time *denormalize-time*})))

(defn db->tree [query data refs]
  (let [ast (fulcro.client.primitives/query->ast query)]
    (binding [*denormalize-time* 22]
      (denormalize ast data refs {}))))

(comment
  (prim/query->ast
    '[:a {:friends [:name {:children [:name]}]}])

  (prim/query->ast '[:a
                     (:b {:x 1})
                     {:rj ...}
                     {:union-join {:union-a [:a {:rj 2}] :union-b [:b {:irj ...}]}}
                     {:c [:d]}])

  (let [state-map {:person {1 {:name   "tony"
                               :spouse [:person 2]}
                            2 {:name     "sam"
                               :spouse   [:person 1]
                               :children [[:person 3] [:person 4]]}
                            3 {:name "judy"}
                            4 {:name "sally"}}}
        ndb->tree fulcro.client.primitives/db->tree]
    (db->tree
      '[:a {:friends [:name {:spouse ...}]}
        {:enemies [:name {:children ...}]}]
      {:a 1 :friends [:person 1] :enemies [:person 2]}
      state-map)))
