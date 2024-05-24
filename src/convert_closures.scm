(define (convert-closures program)
    (define (Lambda lambda_expr)
        (match lambda_expr
            [(,uvar 
                (lambda ,param*
                    (free ,free*
                        ,[Expr -> expr])))
                (let 
                    ([label (unique-label uvar)]
                    [cp (unique-name 'cp)])
                    (values 
                        `(,uvar ,label ,@free*)
                        `(,label
                            (lambda (,cp ,@param*)
                                (bind-free (,cp ,@free*)
                                    ,expr)))))]))
    (define (Proc proc_expr arg*)
        (match proc_expr
            [,uvar (guard (uvar? uvar)) `(,uvar ,uvar ,@arg*)]
            [,x (let 
                    ([uvar (unique-name 'proc)])
                    `(let ([,uvar ,proc_expr])
                        (,uvar ,uvar ,@arg*)))]))
    (define (Expr expr)
        (match expr
            [(quote ,imm) `(quote ,imm)]
            [(if ,[expr*] ...) `(if ,expr* ...)]
            [(begin ,[expr*] ...) `(begin ,expr* ...)]
            [(let ([,uvar* ,[expr*]] ...) ,[sub_expr])
                `(let ([,uvar* ,expr*] ...) ,sub_expr)]
            [(letrec 
                (,[Lambda -> closure* lambda_expr*] ...)
                ,[sub_expr])
                `(letrec
                    (,lambda_expr* ...)
                        (closures ,closure* ,sub_expr))]
            [(,prim ,[expr*] ...) (guard (prim? prim))
                `(,prim ,expr* ...)]
            [(,[proc_expr] ,[expr*] ...) (Proc proc_expr expr*)]
            [,x x]))
    (Expr program))