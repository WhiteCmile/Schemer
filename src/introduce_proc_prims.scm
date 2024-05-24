(define (introduce-procedure-primitives program)
    (define (Closure closure)
        (match closure
            [(,proc ,label ,free* ...)
                (values 
                    `(,proc (make-procedure ,label (quote ,(length free*))))
                    (let loop ([frees free*] [index 0])
                        (match frees
                            [() '()]
                            [(,uvar . ,rest)
                                (cons `(procedure-set! ,proc (quote ,index) ,uvar)
                                    (loop rest (add1 index)))])))]))
    (define (Lambda lambda_expr)
        (match lambda_expr
            [(lambda ,param*
                (bind-free ,free*
                    ,expr))
                `(lambda ,param*
                    ,((Expr (car free*) (cdr free*)) expr))]))
    (define (Expr proc frees)
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
                    `((procedure-code ,proc_call) ,expr* ...)]
                [,uvar 
                    (let 
                        ([index (index-of uvar frees)])
                        (if index
                            `(procedure-ref ,proc (quote ,index))
                            uvar))])))
    ((Expr '() '()) program))