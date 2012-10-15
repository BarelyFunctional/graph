(ns graph.core)

#_(defn update-first [pred f coll]
  (let [[before [at & after]] (split-with (comp not pred) coll)]
    (concat before (cons (f at) after))))

#_(defmacro defnk [& forms]
  `(defn ~@(expand-args forms)))

(defn add-req-ks [ks f] (vary-meta f assoc :req-ks (into #{} ks)))

(defmacro fnk [args & body]
  `(add-req-ks 
     ~(map keyword args)
     (fn [{:keys ~args}] ~@body)))


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

(defn
  graph-walk 
  "graph is map of node -> inputs to node
   returns list of nodes in walk order"
  [graph]
  (->>
	  (iterate 
	    (fn [[ordered graph]]
        (println ordered graph)
	      (let [i (partial clojure.set/intersection (into #{} (keys graph)))]
          (println "after" (first (first (remove (comp not-empty i second) graph))))
	        (if-let [n (first (first (remove (comp not-empty i second) graph)))]
	          [(conj ordered n) (dissoc graph n)]
            (throw (Exception. "Cycles detected in graph!")))))
	    [[] graph])
     (take-while (comp not-empty second))
     rest
     (map first)
   ))

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
