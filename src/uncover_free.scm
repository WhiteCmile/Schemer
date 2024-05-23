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
                    (values (apply append frees*)
                        `(begin ,expr* ...))]
                [(if ,[frees* expr*] ...)
                    (values (apply append frees*)
                        `(if ,expr* ...))]
                [(let ([,uvar* ,[bind_frees* bind_expr*]] ...) ,expr)
                    (let-values 
                        ([(frees sub_expr) ((Expr (union locals uvar*)) expr)])
                        (values (union frees (apply append bind_frees*))
                            `(let ([,uvar* ,bind_expr*] ...)
                                ,sub_expr)))]
                [(letrec ([,uvar* ,[Lambda -> lambda_expr*]] ...)
                    ,[frees sub_expr])
                    (values frees
                        `(letrec ([,uvar* ,lambda_expr*] ...)
                            ,sub_expr))]
                [(,prim ,[frees* expr*] ...) (guard (prim? prim))
                    (values (apply append frees*)
                        `(,prim ,expr* ...))]
                [(,[frees* expr*] ...)
                    (values (apply append frees*)
                        `(,expr* ...))]
                [,uvar 
                    (if (member uvar locals)
                        (values '() uvar)
                        (values `(,uvar) uvar))])))
    (let-values     
        ([(uvars expr) ((Expr '()) expr)])
        expr))