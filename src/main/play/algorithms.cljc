(ns play.algorithms
  #?(:cljs (:require-macros play.algorithms))
  (:require
    [taoensso.tufte :refer [profile p]]
    [fulcro.client.impl.parser :refer [path-meta]]
    [fulcro.client.primitives :as prim]
    [clojure.pprint]))

(defmacro spy [v]
  (let [expr (str v)]
    `(let [result# ~v]
       (println (str ~expr "\n => " (with-out-str (clojure.pprint/pprint result#))))
       result#)))

(defn lookup-ref? [v]
  (and (vector? v) (= 2 (count v)) (not (lookup-ref? (first v)))))

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

(defn reduce-depth
  "Reduce the query depth on `join-node` that appears within the children of `parent-node`."
  [parent-node join-node]
  (p ::reduce-depth
    (let [join-node-index (reduce
                            (fn [idx n] (if (identical? join-node n)
                                          (reduced idx)
                                          (inc idx)))
                            0
                            (:children parent-node))]
      (update-in parent-node [:children join-node-index :query] (fnil dec 1)))))

(defn add-join! [n {:keys [query key] :as join-node} entity state-map parent-node idents-seen]
  (p ::add-join!
    (let [v               (get entity key)
          is-ref?         (lookup-ref? v)
          join-entity     (if is-ref? (get-in state-map v) v)
          to-many?        (and (vector? join-entity) (lookup-ref? (first join-entity)))
          depth-based?    (int? query)
          recursive?      (or (= '... query) depth-based?)
          stop-recursion? (and recursive? (or (= 0 query)
                                            (and is-ref? (contains? (get idents-seen key) v))))
          parent-node     (if (and depth-based? (not stop-recursion?))
                            (reduce-depth parent-node join-node)
                            parent-node)
          target-node     (if recursive? parent-node join-node)
          idents-seen     (if is-ref?
                            (update idents-seen key (fnil conj #{}) v)
                            idents-seen)]
      (cond
        stop-recursion? (assoc! n key v)
        to-many? (assoc! n key
                   (into []
                     (keep (fn [lookup-ref]
                             (when-let [e (get-in state-map lookup-ref)]
                               (denormalize target-node e state-map idents-seen))))
                     join-entity))
        (and recursive? join-entity) (if depth-based?
                                       (let [join-node-index (reduce
                                                               (fn [idx n] (if (identical? join-node n)
                                                                             (reduced idx)
                                                                             (inc idx)))
                                                               0
                                                               (:children parent-node))
                                             parent-node     (update-in parent-node [:children join-node-index :query] (fnil dec 1))]
                                         (assoc! n key (denormalize parent-node join-entity state-map idents-seen)))
                                       (assoc! n key (denormalize parent-node join-entity state-map idents-seen)))
        (map? join-entity) (assoc! n key (denormalize target-node join-entity state-map idents-seen))
        :otherwise n))))

(defn add-union! [n {:keys [key] :as join-node} entity state-map idents-seen]
  (let [v                (get entity key)
        union-node       (-> join-node :children first)
        union-key->query (reduce
                           (fn [result {:keys [union-key] :as node}]
                             (assoc result union-key node))
                           {}
                           (:children union-node))
        to-many?         (and (vector? v) (lookup-ref? (first v)))]
    (cond
      to-many? (assoc! n key
                 (into []
                   (keep (fn [[table :as lookup-ref]]
                           (when-let [e (get-in state-map lookup-ref)]
                             (when-let [target-ast-node (union-key->query table)]
                               (denormalize target-ast-node e state-map idents-seen)))))
                   v))
      (lookup-ref? v) (when-let [e (get-in state-map v)]
                        (when-let [target-ast-node (get union-key->query (first v))]
                          (denormalize target-ast-node e state-map idents-seen)))
      :otherwise n)))

(defn add-joins! [transient-node entity state-map parent-node ast-join-nodes idents-seen]
  (reduce
    (fn [n join-node]
      (let [union? (map? (:query join-node))]
        (if union?
          (add-union! n join-node entity state-map idents-seen)
          (add-join! n join-node entity state-map parent-node idents-seen))))
    transient-node
    ast-join-nodes))

(def ^:dynamic *denormalize-time* 0)

(defn denormalize
  [{:keys [type children] :as top-node} current-entity state-map idents-seen]
  (assert (not= type :prop))
  (let [grouped-children (group-by :type children)
        nil-nodes        (get grouped-children nil false)
        wildcard?        (and nil-nodes (= '* (some-> nil-nodes first :key)))
        result-node      (if wildcard?
                           (transient current-entity)
                           (add-props! (transient {}) current-entity (:prop grouped-children)))
        result-node      (add-joins! result-node current-entity state-map
                           top-node
                           (:join grouped-children)
                           idents-seen)]
    (with-meta (persistent! result-node) {:time *denormalize-time*})))

(defn db->tree [query starting-entity state-map]
  (let [ast (fulcro.client.primitives/query->ast query)]
    (binding [*denormalize-time* 22]
      (denormalize ast starting-entity state-map {}))))

#?(:clj
   (defmacro bench [& body]
     `(time
        (doseq [_# (range 1 1000)]
          ~@body))))

(comment
  (taoensso.tufte/add-basic-println-handler! {})
  (prim/query->ast
    '[:a {:friends [:name {:children 4}]}])

  (prim/query->ast '[{:friends {:employee [:name :role]
                                :person   [:name]}}])

  (prim/query->ast '[*
                     (:b {:x 1})
                     {:rj ...}
                     {:union-join {:union-a [:a {:rj 2}] :union-b [:b {:irj ...}]}}
                     {:c [:d]}])

  (bench
    (db->tree
      '[{:friends {:employee [:name :role]
                   :person   [:name {:children ...}]}}]
      {:friends [[:person 2] [:employee 1]]}
      state-map))

  (let [state-map {:employee {1 {:name "polly" :role "programmer"}}
                   :person   {1 {:name   "tony"
                                 :x      44
                                 :spouse [:person 2]}
                              2 {:name     "sam"
                                 :x        44
                                 :spouse   [:person 1]
                                 :things   [[:person 3] [:employee 1]]
                                 :children [[:person 3] [:person 4]]}
                              3 {:name "judy" :x 99}
                              4 {:name "sally" :x 99}}}
        ndb->tree fulcro.client.primitives/db->tree]
    (play.algorithms/bench
      (db->tree
        '[*
          {:friends [{:spouse ...} :name :x]}
          {:enemies [:name :x {:children ...}
                     {:things {:person [:name] :employee [:role]}}]}]
        {:a       1 :b 2 :c 3 :d 4 :e 5
         :friends [:person 1]
         :enemies [:person 2]}
        state-map))
    ))
