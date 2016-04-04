\* My shen shtuff *\

\* WIP
(defmacro docmacro
  \\ rewrites `define' to not ignore the docstring if there is one.
  [define Name Docstring Sig [Body]] -> [define Name Sig [Body]])
*\

(package bs [inc ack]

(define fizzbuzz
  X -> "FizzBuzz" where (and (divisible-by? X 3)
                             (divisible-by? X 5))
  X -> "Fizz"     where (divisible-by? X 3)
  X -> "Buzz"     where (divisible-by? X 5)
  X -> X)

(tc +)

(define divisible-by?
  { number --> number --> boolean}
  A B -> (integer? (/ A B)))

(define range
  { number -> number -> (list number) }
  I J <- (if (> I J) (error "I must be less than J") (fail))
  I J -> [ I | (range (+ I 1) J)])

(deftype str-or-num
  if (or (string? X) (number? X))
  -----------------------
  X : str-or-num;)


(define inc
  {number --> number}
  N -> (+ N 1))

(define ack
  {number --> number --> number}
  0 N -> (inc N)
  M 0 -> (ack (- M 1) 1)
  M N -> (ack (- M 1)
              (ack M (- N 1))))

(defcc <binary?>
  X <binary?> := true where (element? X [0 1]);
  X           := true where (element? X [0 1]);
  <e>         := false;)


\* SICP *\


(define fib
  {number --> number}
  0 -> 0
  1 -> 1
  N -> (+ (fib (- N 1))
          (fib (- N 2))))

(define fib2
  {number --> number}
  N -> (fib-iter 1 0 N))

(define fib-iter
  {number --> number --> number --> number}
  A B 0 -> B
  A B C -> (fib-iter (+ A B) A (- C 1)))


(define expt
  _ 0 -> 1
  B N -> (* B (expt B (- N 1))))

(define expt2
  B N -> (expt-iter B N 1))

(define expt-iter
  _ 0 P -> P
  B C P -> (expt-iter B (- C 1) (* B P)))


(define fast-expt
  _ 0 -> 1
  B N -> (square (fast-expt B (/ N 2))) where (even? N)
  B N -> (* B (fast-expt B (- N 1)))    where (odd? N))

(define even?
  N -> (= (remainder N 2) 0))

(define gcd {number --> number --> number}
  "Greatest Common Divisor"
  A 0 -> A
  A B -> (gcd B (remainder A B)))

(define smallest-divisor
  N -> (find-divisor N 2))

(define find-divisor
  N Test-divisor -> N            where (> (square Test-divisor) N)
  N Test-divisor -> Test-divisor where (divides? Test-divisor N)
  N Test-divisor -> (find-divisor N (+ Test-divisor 1)))

(define divides?
  A B -> (= (remainder A B) 0))

(define prime?
  N -> (= N (smallest-divisor N)))

(define expmod
  Base 0   Mod -> 1
  Base Exp Mod -> (remainder (square (expmod Base (/ Exp 2) m)) m) where (even? Exp)
  Base Exp Mod -> (remainder (* Base (expmod Base (- Exp 1) m)) m) where (odd? Exp))

(define fermat-test
  N -> (let try-it (/. A (= (expmod A N N) A))
         (try-it (+ 1 (random (- N 1))))))

(define fast-prime?
  N 0 -> true
  N Times -> (fermat-test N) (fast-prime? N (- Times 1))
  _ _ -> false)

\* 1.3 *\

(define cube
  N -> (* N N N))

(define sum-integers*
  A B -> 0 where (> A B)
  A B -> (+ A (sum-integers (+ A 1) B)))

(define sum-cubes*
  A B -> 0 where (> A B)
  A B -> (+ (cube A) (sum-cubes (+ A 1) B)))

(define pi-sum*
  A B -> 0 where (> A B)
  A B -> (+ (/ 1.0 (* A (+ A 2))) (pi-sum (+ A 4) B)))

(define sum Term A Next B -> 0 where (> A B)
            Term A Next B -> (+ (Term A)
                                (sum Term (Next A) Next B)))

(define sum-cubes
  A B -> (sum cube A inc B))

(define identity
  A -> A)

(define sum-integers
  A B -> (sum identity A inc B))

(define pi-sum
  A B -> (let pi-term (/. X (/ 1.0 (* X (+ X 2))))
              pi-next (/. Y (+ Y 4))
           (sum pi-term A pi-next B)))


(define integral
  F A B Dx -> (* (sum F
                      (+ A (/ Dx 2.0))
                      (/. X (+ X Dx))
                      B)
                 Dx))

(define search
  F NegPt PosPt -> )

)
