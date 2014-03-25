(ns app.core)

(use `clojure.pprint)

(defn third [lst] 
  (first (rest (rest lst))))

(defn printdf [& args]
  (binding [*print-suppress-namespaces* true] (apply printf args)))

(defn add [x y]
  (list `add x y))

(defn mul [x y]
  (list `mul x y))

(defn div [x y]
  (list `div x y))

(defn neg [x]
  (list `neg x))

(defn eql [a b]
  (list `eql a b))

(defn associative_op [op]
  (cond
    (= op `eql) true
    (= op `mul) true
    (= op `add) true
    (= op `eqn_and) true
    (= op `eqn_or) true
    :else false))

(defn commutative_op [op]
  (cond
    (= op `eql) true
    (= op `mul) true
    (= op `add) true
    (= op `eqn_and) true
    (= op `eqn_or) true
    :else false))

(defn inv [xpr]
  (cond
    (number? xpr) (list `div 1 xpr)
    :else (list `div (third xpr) (second xpr))))

(defn left_distributive_op [op op2]
  (cond
    (= op2 `eql) true
    (and (= op `mul) (= op2 `add)) true
    (and (= op `eqn_and) (= op2 `eqn_or)) true
    :else false))

(defn right_distributive_op [op op2]
  (cond
    (= op2 `eql) true
    (and (= op `mul) (= op2 `add)) true
    (and (= op `eqn_and) (= op2 `eqn_or)) true
    :else false))

(defn commutative [xpr]
  (def op (first xpr))
  (def x (second xpr))
  (def y (third xpr))
  (cond
    (commutative_op op) (list `eql y x)
    :else xpr))

(defn left_distributive [xpr]
  (def op (first xpr))
  (def x (second xpr))
  (def y (third xpr))
  (def op2 (first y))
  (def x1 (second y))
  (def y1 (third y))
  (cond
    (left_distributive_op op op2) (list op2 (list op x x1) (list op x y1)) 
    :else xpr))

(defn right_distributive [xpr]
  (def op (first xpr))
  (def x (second xpr))
  (def y (third xpr))
  (def op2 (first x))
  (def x1 (second x))
  (def y1 (third x))
  (cond
    (right_distributive_op op op2) (list op2 (list op x1 y) (list op y1 y))
    :else xpr))

(defn right_associative [xpr]
  (def op (first xpr))
  (def x (second xpr))
  (def y (third xpr))
  (def op2 (first x))
  (def x1 (second x))
  (def y1 (third x))
  (cond
    (and (associative_op op) (= op op2)) (list op x1 (list op y1 y)) 
    :else xpr))

(defn left_associative [xpr]
  (def op (first xpr))
  (def x (second xpr))
  (def y (third xpr))
  (def op2 (first y))
  (def x1 (second y))
  (def y1 (third y))
  (cond
    (and (associative_op op) (= op op2)) (list op (list op x x1) y1)
    :else xpr))

(defn left_distributive [xpr]
  (def op (first xpr))
  (def x (second xpr))
  (def y (third xpr))
  (def op2 (first y))
  (def x1 (second y))
  (def y1 (third y))
  (cond
    (left_distributive_op op op2) (list op2 (list op x x1) (list op x y1)) 
    :else xpr))

(defn eqn_and [x y]
  (list `eqn_and x y))

(defn eqn_or [x y]
  (list `eqn_or x y))

(defn eqn_not [x]
  (list `eqn_not x))
 
(defn -main [which]
  (def a0 (add 7 9))
  (def a1 (add 5 a0))
  (def a2 (left_associative a1))
  (def a3 (commutative (add 2 6)))
  (def m1 (mul a2 a3))
  (def m2 (inv 5))
  (def eq1 (eql m1 m2))
  (def eq2 (add 5 eq1))
  (def eq3 (left_distributive eq2))
  (printdf "%s\n" eq3))
