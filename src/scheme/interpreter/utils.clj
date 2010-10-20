(ns scheme.interpreter.utils)

(use 'clojure.test)
;(use 'clojure.test.junit)

(defn make-map-from-pairs
  ([pairs]
    (if (empty? pairs)
      {}
      (reduce 
        #(assoc %1 (first %2) (nth %2 1)) 
        '{}
        pairs
        )))
  {
   :test
   (fn []
       (is (= '{} (make-map-from-pairs '())))
       (is (= '{a 1} (make-map-from-pairs '((a 1)))))
       (is (thrown? Throwable (make-map-from-pairs '((a)))))
       (is (= '{a 1 b 2 c 3} (make-map-from-pairs '((a 1) (b 2) (c 3)))))
     )
   }
  )

(defn map-the-map 
  ([fkey fval the-map]
  (if (empty? the-map)
    {}
    (make-map-from-pairs 
      (map 
        #(list (fkey (key %)) (fval (val %))) 
        the-map))
    ))
  {
   :test
   (fn []
       (is (= '{} (map-the-map nil nil '{})))
       (is (= '{} (map-the-map identity identity '{})))
       (is (= '{a 1 b 2 c 3} (map-the-map identity identity '{a 1 b 2 c 3})))
       (is (= '{a 2 b 4 c 6} (map-the-map identity #(* 2 %) '{a 1 b 2 c 3})))
       (is (= '{ax 1 bx 2 cx 3} (map-the-map #(symbol (str % 'x)) identity '{a 1 b 2 c 3})))
       (is (= '{x 3} (map-the-map (fn [k] 'x) identity '{a 1 b 2 c 3})))
     )
   }
  )

(defn make-map-from-vars-vals 
  ([variables values]
  (if (or (empty? variables) (empty? values))
    {}
    (make-map-from-pairs (map #(list %1 %2) variables values))
    ))
  {
   :test
   (fn []
       (is (= '{} (make-map-from-vars-vals '() '())))
       (is (= '{} (make-map-from-vars-vals '(a) '())))
       (is (= '{} (make-map-from-vars-vals '() '(1))))
       (is (= '{} (make-map-from-vars-vals '(a) nil)))
       (is (= '{} (make-map-from-vars-vals nil '(1))))
       (is (= '{a 1} (make-map-from-vars-vals '(a) '(1 2))))
       (is (= '{a 1} (make-map-from-vars-vals '(a b) '(1))))
       (is (= '{a 1 b 2 c 3} (make-map-from-vars-vals '(a b c) '(1 2 3))))
     )
   }
)

(defn error 
  [msg arg1] 
  (throw ( new Throwable (str msg " " arg1))))

(defn map-nth 
  [f coll] 
  (nth (map f coll) (dec (count coll))))

(defn throw-not-impl 
  [] 
  (throw ( new Throwable "NOT IMPLEMENTED!")))

