;; environments
(define (new-env) '())

(define (force-eval item)
  (if (list? item)
      (let ((evaled-val ((cadr item) (caddr item))))
        (set-cdr! item evaled-val)
        evaled-val)
      (cdr item)))

(define (lookup-var-chk-dep var env dep)
  (if (null? env)
      (begin
        (display dep)
        (newline)
        var)
      (let* ((first (car env))
             (rest (cdr env)))
        (if (eq? var (car first))
            (begin
              (display dep)
              (newline)
              (force-eval first))
            (lookup-var-chk-dep var rest (+ dep 1))))))

(define (lookup-var var env)
  (lookup-var-chk-dep var env 0))

;; Try to find variable var in the environment.
(define (lookup-var var env)
  (if (null? env)
      var ;; Unbounded variable is not an error in lambda calculus.
      (let* ((first (car env))
             (rest (cdr env)))
        (if (eq? var (car first))
            (force-eval first)
            (lookup-var var rest)))))

(define (add-binding var val val-env env)
  (cons (list var val val-env) env))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the main eval function
(define (analyze exp)
  (define (lambda? oper) (eq? oper '^))

  (define (analyze-list seq)
    (if (null? seq)
        '()
        (let* ((head (car seq))
               (tail (cdr seq)))
          (if (eq? head '^)
              (list (analyze-lambda tail))
              (cons (analyze head)
                    (analyze-list tail))))))

  (define (eval-list seq env)
    (map (lambda (x) (x env))
         seq))

  (define (make-proc param body base-env)
    (lambda (param-val) ;; analyzed but unevaluated
      (lambda (param-env)
        (let* ((env (add-binding param
                                 param-val
                                 param-env
                                 base-env)))
             (body env)))))

  (define (analyze-lambda opnd)
    ;; (newline)
    ;; (display "ANALYZE-LAMBDA: ")
    ;; (display opnd)
    ;; (newline)

    (let ((param (car opnd))
          (body (cdr opnd)))
      (if (eq? param '->)
          (analyze body)
          (let* ((analyzed-proc (analyze-lambda body)))
            (lambda (env)
              (make-proc param analyzed-proc env))))))

  ;; eoper == evaluated oper
  ;; aopnd ==  analyzed opnd
  (define (recursive-apply eoper aopnd env)
    (cond ((null? aopnd) eoper)
          ((procedure? eoper) (recursive-apply ((eoper (car aopnd)) env)
                                               (cdr aopnd)
                                               env))
          (else (cons eoper (eval-list aopnd env)))))

  (define (analyze-pair exp)
    (let* ((oper (car exp))
           ;; NOTE: we can't evaluate opnd here because we want normal
           ;; order evaluation.
           (opnd (cdr exp)))
      (cond ((lambda? oper) (analyze-lambda opnd))
            (else (let* ((analyzed-oper (analyze oper))
                         (analyzed-opnd (analyze-list opnd)))
                    (lambda (env)
                      (let* ((evaled-oper (analyzed-oper env)))
                        (recursive-apply evaled-oper analyzed-opnd env))))))))

  (define (analyze-variable exp)
    (lambda (env)
      ;; (newline)
      ;; (display "LOOKUP-VAR: ")
      ;; (display exp)
      ;; (display " ")
      ;; (display env)
      ;; (newline)
      (lookup-var exp env)))


  ;; (newline)
  ;; (display "ANALYZE: ")
  ;; (display exp)
  ;; (newline)
  (cond ((pair? exp) (analyze-pair exp))
        (else (analyze-variable exp))))

(define (my-eval exp env)
  ((analyze exp) env))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define program
  '((^ f b -> b f) (^ f b -> b f)
^ define ->

define (^ x -> x) ^ id ->
define (^ c x -> c) ^ const ->

define (^ f -> (^ x -> f (x x)) (^ x -> f (x x)))
^ fix ->

define (^ x y -> x) ^ true ->
define (^ x y -> y) ^ false ->
define (^ b x y -> b y x) ^ not ->
define (^ a b -> a b false) ^ and ->
define (^ a b -> a true b) ^ or ->

define (^ x y f -> f x y) ^ cons ->
define (^ p -> p true) ^ car ->
define (^ p -> p false) ^ cdr ->
define false ^ nil ->
define (^ p -> p (^ _ _ _ -> false) true) ^ isNil ->
define (^ p -> not (isNil p)) ^ isPair ->

define (fix (^ map f xs -> (isNil xs) nil (cons (f (car xs)) (map f (cdr xs)))))
^ map ->

define (fix (^ filter f xs ->
	(isNil xs) nil ((f (car xs)) (cons (car xs) (filter f (cdr xs)))
                                 (filter f (cdr xs)))))
^ filter ->

define (fix (^ foldr step init xs ->
	(isNil xs) init (step (car xs) (foldr step init (cdr xs)))))
^ foldr ->

define (^ f x -> x) ^ 0 ->
define (^ n f x -> f (n f x)) ^ succ ->

define (succ 0) ^ 1 ->
define (succ 1) ^ 2 ->
define (succ 2) ^ 3 ->
define (succ 3) ^ 4 ->
define (succ 4) ^ 5 ->
define (succ 5) ^ 6 ->
define (succ 6) ^ 7 ->
define (succ 7) ^ 8 ->
define (succ 8) ^ 9 ->
define (succ 9) ^ 10 ->

define (^ n f x -> (n (^ h g -> g (h f)) (^ g -> x) id)) ^ prec ->

define (^ n m -> n succ m) ^ add ->
define (^ n m -> m prec n) ^ minus ->
define (^ n m f -> m (n f)) ^ mult ->
define (^ n m -> m (mult n) 1) ^ exponential ->

define (^ n -> n (const true) false) ^ isPositive ->
define (^ n -> n (const false) true) ^ isZero ->
define (^ n m -> and (isZero (minus n m)) (isZero (minus m n))) ^ equal ->
define (^ n m -> or (isPositive (minus n m)) (isPositive (minus m n))) ^ notEqual ->

define (fix (^ len xs -> (isNil xs) 0 (succ (len (cdr xs))))) ^ length ->

define (fix (^ fac n -> (isZero n) 1 (mult n (fac (prec n))))) ^ factorial ->

define (fix (^ gen n -> (isZero n) nil (cons n (gen (prec n))))) ^ genList ->

define ((fix (^ isOk n ks k -> (isNil ks)
    true
    (and (notEqual k (car ks))
    (and (notEqual (add n k) (car ks))
    (and (notEqual k (add n (car ks)))
         (isOk (succ n) (cdr ks) k)))))) 1)
^ isValidNQ ->

define ((fix (^ isOk n ks k -> (isNil ks)
    true
    (and (notEqual k (car ks))
         (isOk (succ n) (cdr ks) k)))) 1)
^ isValidNR ->

define (^ size -> (fix (^ search n ks ->
    (isZero n)
        (cons ks)
        (foldr (^ f g x -> f (g x)) id (map (search (prec n))
            (map (^ k -> cons k ks) (filter (isValidNQ ks) (genList size)))))))
	size nil nil)
^ searchNQ ->

define (^ size -> (fix (^ search n ks ->
    (isZero n)
        (cons ks)
        (foldr (^ f g x -> f (g x)) id (map (search (prec n))
            (map (^ k -> cons k ks) (filter (isValidNR ks) (genList size)))))))
	size nil nil)
^ searchNR ->

length (searchNQ 6) f x
;;length (genList 4) f x
;;factorial 3 f x
))

;; (define program
;;   '((^ x -> y) ((^ x -> x x) (^ x -> x x))))

(display (my-eval program (new-env)))
(newline)
