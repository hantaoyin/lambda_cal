;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the main eval function
(define (compile exp)
  (define (make-thunk p env)
    (lambda (a)
      (let ((b (p env)))
        (set! a (lambda (c) b))
        b)))

  (define (lookup var env)
    (if (eq? var 0)
        ((car env) (car env))
        (lookup (- var 1) (cdr env))))

  (define (compile-var exp)
    (lambda (env)
      (lookup exp env)))

  (define (compile-lam exp)
    (let ((compiled-exp (compile exp)))
      (lambda (env)
        (lambda (param)
          (compiled-exp (cons param env))))))

  (define (compile-app exp)
    (let* ((f (compile (car exp)))
           (p (compile (cadr exp))))
      (lambda (env)
        (let* ((fv (f env))
               (pv (make-thunk p env)))
          (if (procedure? fv)
              (fv pv)
              (list fv (p env)))))))

  (define (compile-pair exp)
    (if (null? exp)
        (error "FIXME!")
        (let* ((head (car exp))
               (tail (cdr exp)))
          (cond ((eq? head 'lam) (compile-lam (car tail)))
                ((eq? head 'app) (compile-app tail))
                (else (error "FIXME!"))))))
  
  (cond ((pair? exp) (compile-pair exp))
        ((number? exp) (compile-var exp)) ;; bounded var
        (else (lambda (env) exp)) ;; unbounded var
        ))

(define (my-eval exp env)
  ((compile exp) env))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; evaluate from a file
(define (load-program)
  (let ((obj (read)))
    (if (eof-object? obj)
        '()
        (cons obj (load-program)))))

(define (parse prog)

  (define (parse-lam prog)
    (let* ((head (car prog))
           (tail (cdr prog)))
      (cond ((eq? head '->) (parse-app tail))
            ((pair? head) (error "Parse error."))
            (else (list 'lam head (parse-lam tail))))))

  (define (parse-app prog)
    (define (acc ret item)
      (if (null? ret)
          item
          (list 'app ret item)))

    (define (helper ret prog)
      (if (null? prog)
          ret
          (let ((head (parse (car prog)))
                (tail (cdr prog)))
            (if (eq? head '^)
                (acc ret (parse-lam tail))
                (helper (acc ret head)
                        tail)))))
    (helper '() prog))

  (if (pair? prog) ;; symbol or not?
      (parse-app prog)
      prog))

;; translate to the De Bruijn notation
(define (to-de-bruijn prog)
  (define (lookup ret env sym)
    (cond ((null? env) sym)
          ((eq? (car env) sym) ret)
          (else (lookup (+ ret 1) (cdr env) sym))))

  (define (helper env prog)
    ;; (display prog)
    ;; (newline)
    (if (null? prog)
        '()
        (if (pair? prog)
            (let* ((head (car prog))
                   (tail (cdr prog)))
              (cond ((eq? head 'lam) (list 'lam (helper (cons (car tail) env) (cadr tail))))
                    ((eq? head 'app) (list 'app
                                           (helper env (car tail))
                                           (helper env (cadr tail))))
                    ((else (cons (helper env head)
                                 (helper env tail))))))
            (lookup 0 env prog))))

  (helper '() prog))

(define test-program '((^ x y -> x x z) a b))

;;(display (my-eval (to-de-bruijn (parse test-program)) '()))
(display (my-eval (to-de-bruijn (parse (load-program))) '()))
(newline)
