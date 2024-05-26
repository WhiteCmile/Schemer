(define (purify-letrec program)
    ; Partition all the bindings into three sets
    (define (partition_bindings letrec_uvar* bind_expr* assigned_vars)
        ; Check whether the bind_expr is a lambda expression
        (define (lambda? uvar bind_expr)
            (match bind_expr
                [(lambda ,useless* ...) (guard (not (member uvar assigned_vars))) #t]
                [,x #f]))
        ; Check whether the bind_expr is a simple expression
        ; A expression is seen as a simple expression if it is
        ;   1. a simple constant
        ;   2. a variable that is not bound by letrec
        ;   3. a primitive call with simple operands
        (define (simple? uvar bind_expr)
            (if (member uvar assigned_vars)
                #f
                (letrec 
                    ([func 
                        (lambda (bind_expr)
                            (match bind_expr
                                [(,prim ,expr* ...) (guard (prim? prim)) 
                                    (andmap func expr*)]
                                [(quote ,x) (guard (immediate? x)) #t]
                                [,x (guard (uvar? x)) (not (member x letrec_uvar*))]
                                [,x #f]))])
                    (func bind_expr))))
        (let loop ([uvar* letrec_uvar*] [bind_expr* bind_expr*])
            (match uvar*
                [() (values '() '() '() '() '() '())]
                [(,uvar . ,rest)
                    (let-values 
                        ([(simple_x simple_e lambda_x lambda_e complex_x complex_e) 
                            (loop rest (cdr bind_expr*))])
                        (cond
                            [(lambda? uvar (car bind_expr*))
                                (values 
                                    simple_x simple_e 
                                    (cons uvar lambda_x) (cons (car bind_expr*) lambda_e)
                                    complex_x complex_e)]
                            [(simple? uvar (car bind_expr*))
                                (values 
                                    (cons uvar simple_x) (cons (car bind_expr*) simple_e)
                                    lambda_x lambda_e
                                    complex_x complex_e)]
                            [else
                                (values 
                                    simple_x simple_e 
                                    lambda_x lambda_e
                                    (cons uvar complex_x) (cons (car bind_expr*) complex_e))]))])))
    ; Turn impure letrec to pure letrec
    (define (handle_letrec uvar* bind_expr* assigned_vars body_expr)
        (let-values 
            ([(simple_x simple_e
                lambda_x lambda_e
                complex_x complex_e)
                (partition_bindings uvar* bind_expr* assigned_vars)])
            (let ([temp_x (map (lambda (x) (unique-name 'tmp_x)) complex_x)])
                `(let ,(map list simple_x simple_e)
                    (assigned ()
                        (let ,(map (lambda (x) (list x '(void))) complex_x)
                            (assigned ,complex_x
                                (letrec ,(map list lambda_x lambda_e)
                                    (let ,(map list temp_x complex_e)
                                        (assigned ()
                                            ,(make-begin
                                                `(,@(map (lambda (x_c x_t) `(set! ,x_c ,x_t)) complex_x temp_x)
                                                ,body_expr))))))))))))
    (define (Expr expr)
        (match expr
            [(letrec ([,uvar* ,[Expr -> bind_expr*]] ...)
                (assigned ,assigned_vars
                    ,[Expr -> body_expr]))
                (handle_letrec uvar* bind_expr* assigned_vars body_expr)]
            [(,[Expr -> expr*] ...) `(,@expr*)]
            [,x x]))
    (Expr program))