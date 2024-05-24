(define (remove-anonymous-lambda program)
    (define (Expr is_anonymous)
        (lambda (expr)
            (define Expr_f (Expr #f))
            (define Expr_t (Expr #t))
            (match expr
                [(if ,[Expr_t -> expr*] ...) `(if ,@expr*)]
                [(begin ,[Expr_t -> expr*] ...) `(begin ,@expr*)]
                [(lambda ,uvar* ,[Expr_t -> sub_expr])
                    (if is_anonymous
                        (let ([anon (unique-name 'anony)])
                            `(letrec ([,anon (lambda ,uvar* ,sub_expr)])
                                ,anon))
                        `(lambda ,uvar* ,sub_expr))]
                [(let ([,uvar* ,[Expr_f -> expr*]] ...) ,[Expr_t -> sub_expr])
                    `(let ([,uvar* ,expr*] ...) ,sub_expr)]
                [(letrec ([,uvar* ,[Expr_f -> expr*]] ...) ,[Expr_t -> sub_expr])
                    `(letrec ([,uvar* ,expr*] ...) ,sub_expr)]
                [(,[Expr_t -> expr*] ...) `(,@expr*)]
                [,x x])))
    ((Expr #t) program))
