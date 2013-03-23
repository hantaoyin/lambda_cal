;; environments
;;
;; Environment is implemented as a list of items. Each evalueted item
;; is a (symbol . value) pair, where value has been evaluated and
;; ready for use. An item can also be a list: (symbol definition
;; definition-environment) if it has not been evaluated yet.
;;
;; The first time we look up some variable, we evaluate it by
;; executing (definition definition-environment). After that we remove
;; both definition and definition-environment and replace them by the
;; value obtained.
(define (new-env) '())

(define (force-eval item)
  (if (list? item)
      (let ((evaled-val ((cadr item) (caddr item))))
        (set-cdr! item evaled-val)
        evaled-val)
      (cdr item)))

;; (define (lookup-var-chk-dep var env dep)
;;   (if (null? env)
;;       (begin
;;         (display dep)
;;         (newline)
;;         var)
;;       (let* ((first (car env))
;;              (rest (cdr env)))
;;         (if (eq? var (car first))
;;             (begin
;;               (display dep)
;;               (newline)
;;               (force-eval first))
;;             (lookup-var-chk-dep var rest (+ dep 1))))))

;; (define (lookup-var var env)
;;   (lookup-var-chk-dep var env 0))

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
;; evaluate from a file
(define (load-program)
  (let ((obj (read)))
    (if (eof-object? obj)
        '()
        (cons obj (load-program)))))

;; For test purpose program can also be defined here.
;;
;; (define program
;;   '((^ x -> y) ((^ x -> x x) (^ x -> x x))))

;;(display (load-program))
(display (my-eval (load-program) (new-env)))
(newline)
