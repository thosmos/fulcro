(ns play.main
  (:require
    [fulcro.client.primitives :as prim]
    ["react" :as react :refer [useState useEffect]]
    ["react-dom" :as react-dom]
    [fulcro.client.dom :as dom]))

(defonce app-db (atom {:counter/id {1 {:counter/id 1 :counter/n 1}
                                    2 {:counter/id 2 :counter/n 10}}}))
(defonce index (atom {}))

(defn get-query [c] (.-fulcro_query c))
(defn get-props [this] (.-fulcro_props this))
(defn get-ident
  ([this] ((.-fulcro_ident this) (get-props this)))
  ([c props] ((.-fulcro_ident c) props)))

(defn index! [ident setProps]
  (swap! index update ident (fnil conj #{}) setProps))

(defn drop! [ident setProps]
  (swap! index update ident disj setProps))

(defn use-state [default-value]
  (let [result (useState default-value)]
    [(aget result 0) (aget result 1)]))

(defn use-effect
  ([f] (useEffect f))
  ([f deps] (useEffect f (clj->js deps))))

(defn use-fulcro
  "React Hook to simulate hooking fulcro state database up to a hook-based react component."
  [component query ident-fn initial-ident]
  (let [[props setProps] (use-state {})] ; this is how the component gets props, and how Fulcro would update them
    (use-effect ; the empty array makes this a didMount effect
      (fn []
        (set! (.-fulcro_query component) query) ;; record the query and ident function on the component function itself
        (set! (.-fulcro_ident component) ident-fn)
        ;; pull initial props from the database, and set them on the props
        (let [initial-props (prim/db->tree query (get-in @app-db initial-ident) @app-db)]
          (setProps initial-props))
        ;; Add the setProps function to the index so we can call it later (set of functions stored by ident)
        (index! initial-ident setProps)
        ;; cleanup function: drops the update fn from the index
        (fn []
          (drop! initial-ident setProps)))
      #js [])
    props))

;; This is kind of what defsc would generate...nested props take a little more work
(defonce Counter
  ;; here the idea is that we're able to define a root without an incoming join edge. This lets any component act as
  ;; a data root, even though it isn't the app root
  (fn [config-props]
    (let [{:counter/keys [id n] :as props} (use-fulcro
                                             Counter        ; component
                                             [:counter/id :counter/n] ; query
                                             (fn [p] [:counter/id (:counter/id p)]) ; ident-fn
                                             [:counter/id (aget config-props "mount-id")])] ; initial-ident
      (dom/div
        (dom/div "Counter " id)
        (dom/button {:onClick (fn incr* []
                                (let [;; MUTATE
                                      ident         [:counter/id (aget config-props "mount-id")]
                                      _             (swap! app-db update-in (conj ident :counter/n) inc)
                                      ;; REFRESH is trivial..denormalize and set props via index stored set props functions
                                      query         (get-query Counter)
                                      props         (prim/db->tree query (get-in @app-db ident) @app-db)
                                      set-props-fns (get @index ident)]
                                  ;; Refreshes all components with the given ID.
                                  (doseq [set-props set-props-fns]
                                    (set-props props))))}
          (str n))))))

(defn ui-counter [props]
  (dom/create-element Counter props))

(defn render! [c]
  (.render react-dom c (.getElementById js/document "app"))
  :done)


(comment
  (render!
    (dom/div
      (ui-counter #js {:mount-id 1})
      (ui-counter #js {:mount-id 2})
      (ui-counter #js {:mount-id 1})))

  index)
