# Misp -- A Minimal Lisp Implementation in Haskell

```
(def (fib x)
  (if (< x 2)
      x
      (+ (fib (- x 1))
         (fib (- x 2)))))

(println (fib 10))
```
