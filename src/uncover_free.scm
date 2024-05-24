(define (uncover-free expr)
    (define (Lambda lambda_expr)
        (match lambda_expr
            [(lambda ,uvar* ,expr)
                (let-values 
                    ([(frees expr) ((Expr uvar*) expr)])
                    `(lambda ,uvar*
                        (free ,frees
                            ,expr)))]))
    (define (Expr locals)
        (lambda (expr)
            (match expr
                [(quote ,imm) (values '() `(quote ,imm))]
                [(begin ,[frees* expr*] ...)
                    (values (apply union frees*)
                        `(begin ,expr* ...))]
                [(if ,[frees* expr*] ...)
                    (values (apply union frees*)
                        `(if ,expr* ...))]
                [(let ([,uvar* ,[bind_frees* bind_expr*]] ...) ,expr)
                    (let-values 
                        ([(frees sub_expr) ((Expr (union locals uvar*)) expr)])
                        (values (union frees (apply union bind_frees*))
                            `(let ([,uvar* ,bind_expr*] ...)
                                ,sub_expr)))]
                [(letrec ([,uvar* ,[Lambda -> lambda_expr*]] ...)
                    ,sub_expr)
                    (let-values 
                        ([(frees sub_expr) ((Expr (union locals uvar*)) sub_expr)])
                        (values frees
                            `(letrec ([,uvar* ,lambda_expr*] ...)
                                ,sub_expr)))]
                [(,prim ,[frees* expr*] ...) (guard (prim? prim))
                    (values (apply union frees*)
                        `(,prim ,expr* ...))]
                [(,[frees* expr*] ...)
                    (values (apply union frees*)
                        `(,expr* ...))]
                [,uvar 
                    (if (member uvar locals)
                        (values '() uvar)
                        (values `(,uvar) uvar))])))
    (let-values     
        ([(uvars expr) ((Expr '()) expr)])
        expr))