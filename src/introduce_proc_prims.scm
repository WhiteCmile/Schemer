(define (introduce-procedure-primitives program)
    (define (Lambda lambda_expr)
        (match lambda_expr
            [(lambda ,param*
                (bind-free ,free*
                    ,expr))
                `(lambda ,param*
                    ,((Expr (car free*) (cdr free*)) expr))]))
    (define (Expr proc frees)
        (define (Closure closure)
            (match closure
                [(,proc_call ,label ,free* ...)
                    (values 
                        `(,proc_call (make-procedure ,label (quote ,(length free*))))
                        (let loop ([free* free*] [index 0])
                            (match free*
                                [() '()]
                                [(,uvar . ,rest)
                                    (cons `(procedure-set! ,proc_call (quote ,index) ,((Expr proc frees) uvar))
                                        (loop rest (add1 index)))])))]))
        (lambda (expr)
            (match expr
                [(quote ,imm) `(quote ,imm)]
                [(if ,[expr*] ...) `(if ,expr* ...)]
                [(begin ,[expr*] ...) `(begin ,expr* ...)]
                [(let ([,uvar* ,[expr*]] ...) ,[sub_expr])
                    `(let ([,uvar* ,expr*] ...) ,sub_expr)]
                [(letrec 
                    ([,uvar* ,[Lambda -> lambda_expr*]] ...)
                        (closures (,[Closure -> make_stmt* set_stmt*] ...)
                            ,[sub_expr]))
                    `(letrec 
                        ([,uvar* ,lambda_expr*] ...)
                            (let ,make_stmt*
                                ,(make-begin `(,@(apply append set_stmt*) ,sub_expr))))]
                [(,prim ,[expr*] ...) (guard (prim? prim))
                    `(,prim ,expr* ...)]
                [(,[proc_call] ,[expr*] ...) 
                    (if (label? proc_call)
                        `(,proc_call ,@expr*)
                        `((procedure-code ,proc_call) ,expr* ...))]
                [,uvar 
                    (let 
                        ([index (index-of uvar frees)])
                        (if index
                            `(procedure-ref ,proc (quote ,index))
                            uvar))])))
    ((Expr '() '()) program))