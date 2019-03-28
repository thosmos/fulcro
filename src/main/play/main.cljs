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

(defn use-fulcro [component query ident-fn initial-ident]
  (let [[props setProps] (use-state {})]
    (use-effect
      (fn []
        (set! (.-fulcro_query component) query)
        (set! (.-fulcro_ident component) ident-fn)
        (let [initial-props (prim/db->tree query (get-in @app-db initial-ident) @app-db)]
          (setProps initial-props))
        (index! initial-ident setProps)
        (fn []
          (drop! initial-ident setProps)))
      #js [])
    props))

(defonce Counter
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
                                      ;; REFRESH
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
