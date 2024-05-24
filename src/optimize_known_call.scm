(define (optimize-known-call program)
    (define (Expr known_closures can_replace)
        (lambda (expr)
            (define Proc (Expr known_closures #t))
            (define NotProc (Expr known_closures #f))
            (match expr
                [(quote ,imm) `(quote ,imm)]
                [(begin ,[NotProc -> expr*] ... ,[tail]) `(begin ,@expr* ,tail)]
                [(if ,[NotProc -> pred] ,[conseq] ,[alter]) `(if ,pred ,conseq ,alter)]
                [(let ([,uvar* ,[NotProc -> expr*]] ...) ,[sub_expr])
                    `(let ([,uvar* ,expr*] ...) ,sub_expr)]
                [(letrec ([,uvar* ,expr*] ...)
                    (closures ,closure*
                        ,sub_expr))
                    (let*
                        ([known_closures 
                            (append known_closures 
                                (map (lambda (closure_vars)
                                        (list (car closure_vars) (cadr closure_vars)))
                                    closure*))]
                        [expr* (map (Expr known_closures #f) expr*)])
                        `(letrec ([,uvar* ,expr*] ...) 
                            (closures ,closure*
                                ,((Expr known_closures can_replace) sub_expr))))]
                [(lambda ,param*
                    (bind-free ,free*
                        ,[sub_expr]))
                    `(lambda ,param* (bind-free ,free* ,sub_expr))]
                [(,prim ,[NotProc -> expr*] ...) (guard (prim? prim))
                    `(,prim ,@expr*)]
                [(,[Proc -> proc] ,[NotProc -> expr*] ...)
                    `(,proc ,@expr*)]
                [,uvar (guard (uvar? uvar))
                    (let ([closure (assoc uvar known_closures)])
                        (if (and closure can_replace)
                            (cadr closure)
                            uvar))]
                [,x (error 'Expr "Unknown expression ~s\n" expr)])))
    ((Expr '() #f) program))