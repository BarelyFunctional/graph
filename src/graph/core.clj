(ns graph.core
  (:use graph.form-analysis))

; for each value, find the free-symbols, throw away the namespaced symbols
; collect all resolvable symbols, make these optional, all unresolved are required
; use of delays
; replace all symbols with (force symbol)
; use special flag to signify var like $ ?
; a var is not namespaced and starts with $, may have default values

;; output a map of keywords to fnk's

(defn add-req-ks [ks f] (vary-meta f assoc :req-ks (into #{} ks)))

(defmacro fnk [args & body]
  `(add-req-ks 
     ~(map keyword args)
     (fn [{:keys ~args}] ~@body)))

(defn resolved? [sym] (boolean (.getNamespace sym)))

(defmacro with-each [x xs f]
  `(vary-meta
    (fn [m#] (map #(f (assoc m# ~(keyword x) (force %))) ~xs))
    merge (update-in (meta ~f) [:req-ks] conj ~xs)))

(defmacro dfn [body]
  (let [params (remove #(or (resolved? %) (resolve %)) (free-symbols body))
        smap   (zipmap params (map (partial list 'force) params))]
    `(fnk ~(vec params) (delay ~(form-replace smap body)))))

(defmacro eq [name body] `(def ~name (dfn ~body)))

(def evens (partial take-nth 2))
(def odds (comp evens rest))

(defn wrap-form [form]
  (cond
    (and (seq? form) (not-empty form) (= (first form) 'with-each)) form
    (coll? form) (list 'dfn form)
    :else form))

(defmacro deps [& kvs]
  (zipmap
   (map keyword (evens kvs))
   (map wrap-form (odds kvs))))

(defmacro defnk [& fdecl]
  (let [[before [args & after]] (split-with (comp not vector?) fdecl)
        req-meta {:req-ks (into #{} (map keyword args))}
        before (if (map? (last before))
                 (concat (butlast before) (list (merge (last before) req-meta)))
                 (concat before (list req-meta)))]
    `(defn ~@(concat before (list [{:keys args}]) after))))

(defn defnk? [v]
  (and (var? v) (fn? (var-get v)) (contains? (meta v) :req-ks))) 

(defn ns-graph [ns]
  (->> (ns-map ns)
    (filter (comp defnk? second))
    (map (fn [[s v]] [(keyword s) (vary-meta (var-get v) merge (meta v))]))
    (into {})))

; to compile a graph?
; given a graph, what are the required inputs?
(defn required-keys [graph]
  (clojure.set/difference
    (->> graph vals (mapcat :req-ks) (into #{}))
    (into #{} (keys graph))))

(defn roots [graph]
  (let [i (partial clojure.set/intersection (into #{} (keys graph)))]
    (remove (comp not-empty i second) graph)))

(defn graph-walk 
  "graph is map of node -> inputs to node
   returns list of nodes in walk order"
  [graph]
  (let [e (fn [] (throw (Exception. "Cycles detected in graph!")))        
        r #(if (not-empty %) (first (first %)) (e))        
        p #(if (not-empty %) (let [n (r (roots %))] [n (dissoc % n)]))]
    (->>
      [nil graph]
      (iterate (comp p second))
      (take-while identity)
      rest
      (map first))))

(defn map-vals [f m] (zipmap (keys m) (map f (vals m))))

; detect cycles in graph, throw exception if detected
(defn linearize-graph [graph]
  (let [ordered (graph-walk (map-vals (comp :req-ks meta) graph))]
    (map vector ordered (map graph ordered))))

(defn compile-eager-graph
  "Returns a fnk which takes a map as input and produces a map as output"
  [graph]
  (let [ordered (doall (linearize-graph graph))
        reduce-fn (fn [m [k f]] (assoc m k (f m)))]
    (add-req-ks (required-keys graph)
      (fn [m] (reduce reduce-fn m ordered)))))

; each function has optional values

; credit-curves (with-each curve-id credit-curve-ids credit-curve)


  ; compute an order to work out the keys, start out with the required keys
  ; work out the required inputs?

#_(defn compile-lazy-graph
  "Returns a fnk which takes a map as input and produces a map as output"
  [graph]
  (into #{} (mapcat :req-ks (vals graph)))

  ; work out the required inputs?
  )

; call a method with a map

; pass a function to the compilation step
; returns a function which will generate the correct result 
; given an input map

;(compile-lazy )

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
