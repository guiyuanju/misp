(def assert (lambda (name x)
    (if x
        (println (++ name ": ok"))
        (println (++ name ": failed")))))

(assert "" (= 1 2))

;; while
(def a 10)
(while (> a 0)
    (set a (- a 1))
    (println a))

;; println 
(println (+ 1 2))

;; define 
(def a 2) (def b 10)

;; function 
(def add (lambda (a b) (+ a b))) (println (add a b))

;; inner define ;; multiple statements ;; shadow 
(def add2 (lambda (x y) (def a 10) (println "inner add2" (- a y)) (+ a y))) (println (add2 a b))

;; closure
(def counter (lambda () (def variable 0) (def inner (lambda (x) (+ variable x))) inner)) (println ((counter) 110))

;; set
(def aa 10) (println aa) (set aa 100) (println aa)

;; if
(if true (println "true") (println "false")) (if false (println "true") (println "false"))

;; comparison
(println (> 10 1)) (println (< 10 1)) (println (= 10 10)) (println (>= 10 10)) (println (<= 10 1))

;; fib
(def fib (lambda (n)
    (if (< n 2)
        n
        (+ (fib (- n 1))
            (fib (- n 2))))))

(println "prog")
(prog
    (println 1)
    (println 2)
    (def a 222)
    (println a))

(def forfib (lambda (x f)
    (if (= x 0)
     nil
     (prog
        (println (f x))
        (forfib (- x 1) f)))))

(println (forfib 10 fib))

(error "hi")

;; logical
(println not)
(println (and true false))
(println (or true false))
(println (not true))
