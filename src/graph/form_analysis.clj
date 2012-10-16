(ns graph.form-analysis
  (:use clojure.set))

(declare free-symbols)

(defn all-symbols [form] (into #{} (filter symbol? (flatten [form]))))

(defn free-coll-symbols [coll] (apply union (map free-symbols coll)))

(defn free-params-body-symbols [[params & body]]
  (difference 
    (free-coll-symbols body)
    (all-symbols params)))

(defn free-fn-symbols* [fn-tail]
  (apply union (map free-params-body-symbols fn-tail)))

(defn free-fn-symbols [fn-tail]
  (if (symbol? (first fn-tail))
    (difference (free-fn-symbols* (rest fn-tail)) #{(first fn-tail)})
    (free-fn-symbols* fn-tail)))

(defn free-binding-symbols [bindings]
  (first
  (reduce
    (fn [[free bound] [lhs rhs]]
      [(union free (difference (free-symbols rhs) bound))
       (union bound (all-symbols lhs))])
    nil
    (partition 2 bindings))))

(defn free-letfn-symbols [[fns & body]]
  (difference
   (apply union
          (free-coll-symbols body)
          (map free-symbols (take-nth 2 (rest fns))))
   (into #{} (take-nth 2 fns))))

(defn free-let-symbols [[bindings & body]]
  (union
    (free-binding-symbols bindings)
	  (difference
	    (apply union (map free-symbols body))
      (all-symbols (take-nth 2 bindings)))))

(defn free-symbols [form]
  (let [form (macroexpand form)]
	  (cond
	    (symbol? form) #{form} 
	    (and (coll? form) (not-empty form))
	      (if (seq? form)
	        (condp = (first form)
	          'fn*    (free-fn-symbols (rest form))
	          'let*   (free-let-symbols (rest form))
                  'letfn* (free-letfn-symbols (rest form))
	          'loop*  (free-let-symbols (rest form))
                  'if     (free-coll-symbols (rest form))
                  (free-coll-symbols form))
	        (free-coll-symbols form))
	    :else #{})))

; q1 does the unexpanded form contain any of the symbols?
; q2 does the expanded form define a replacement for any of the symbols?
; which symbols are free?
; re-write expression to preclude shadowing
; all found attributes are optional
; place a test to see if any optional attributes are defined, if not fast-path
; given map see if there is any intersection
; when wiring can wire fast path and slow paths
; when graph is compiled can compute all possible optional values for each node
; initial entry point checks to see if input contains any optional value

; Q1 is there any node in the graph which can satisfy optional value?

; simple - any bound value to a existing symbol "cannot" be overriden
; get performance + deterministic behaviour

;; any parameters
#_(defn fn-replace [fntail smap]
  (concat
   (take-while (comp not seq?) fntail)
   ()
   )
  )

;; for a function what are the free symbols?

;; TODO needs to be expanded so shadowed vars won't be replaced
(defn form-replace [smap form]
  (clojure.walk/postwalk-replace smap form))

#_(defn form-replace1 [smap form]
  (cond
   (contains? smap form) (get smap form)
   (coll? form) (cond
                 (empty? form) form
                 (seq? from) (condp = (first (macroexpand form))
                               'fn* nil
                               'let* nil
                               'letfn* nil
                               
                               )
                 (into (empty form) (map (partial form-replace1 smap) form)))
   form

          
    (and (coll? form) (not-empty form))
       (if (seq? form)
	 (condp = (first form)
	   'fn*   (free-fn-symbols (rest form))
	   'let*  (free-let-symbols (rest form))
           
	   'loop* (free-let-symbols (rest form))
           'if    (free-coll-symbols (rest form))
                  (free-coll-symbols form))
                
     ))
  )

#_(defn form-replace [form smap]  
  (let [smap (apply select-keys smap
                          (intersection
                           (free-symbols form)
                           (into #{} (keys smap))))]
    (cond
     (symbol? form) (get smap form form)
     (and (coll? form) (not-empty form))
       (if (seq? form)
	 (condp = (first form)
	   'fn*   (free-fn-symbols (rest form))
	   'let*  (free-let-symbols (rest form))
	   'loop* (free-let-symbols (rest form))
           'if    (free-coll-symbols (rest form))
                  (free-coll-symbols form))
                
     )
    
    )
))

