(define (sanitize-binding-forms program)
    (define (extract_lambda_bindings uvar* expr* sub_expr)
        (define let_bindings '())
        (define letrec_bindings '())
        (let loop 
            ([uvar* uvar*] [expr* expr*])
            (match uvar*
                [() '()]
                [(,uvar . ,rest)
                    (match (car expr*)
                        [(lambda ,params* ,[sanitize-binding-forms -> lambda_body])
                            (set! letrec_bindings 
                                (cons
                                    (list uvar `(lambda ,params* ,lambda_body))
                                    letrec_bindings))]
                        [,expr (set! let_bindings (cons (list uvar (sanitize-binding-forms expr)) let_bindings))])
                    (loop rest (cdr expr*))]))
        (cond
            [(null? let_bindings) `(letrec ,letrec_bindings ,sub_expr)]
            [(null? letrec_bindings) `(let ,let_bindings ,sub_expr)]
            [else `(letrec ,letrec_bindings
                        (let ,let_bindings ,sub_expr))]))
    (match program
        [(begin ,[expr*] ...) (make-begin expr*)]
        [(if ,[expr*] ...) `(if ,@expr*)]
        [(letrec () ,[sub_expr]) sub_expr]
        [(let () ,[sub_expr]) sub_expr]
        [(letrec ([,uvar* (lambda ,params* ,[lambda_body*])] ...) ,[sub_expr])
            `(letrec ([,uvar* (lambda ,params* ,lambda_body*)] ...) ,sub_expr)]
        [(let ([,uvar* ,expr*] ...) ,[sub_expr])
            (extract_lambda_bindings uvar* expr* sub_expr)]
        [(,[expr*] ...) `(,@expr*)]
        [,x x]))