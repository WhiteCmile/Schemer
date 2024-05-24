(define (remove-anonymous-lambda program)
    (define (Expr is_anonymous)
        (lambda (expr)
            (match expr
                [(if ,[expr*] ...) `(if ,@expr*)]
                [(begin ,[expr*] ...) `(begin ,@expr*)]
                [(lambda ,uvar* ,[(Expr #f) -> sub_expr])
                    (if is_anonymous
                        (let ([anon (unique-name 'anony)])
                            `(letrec ([anon (lambda ,uvar* ,sub_expr)])
                                anon))
                        `(lambda ,uvar* ,sub_expr))]
                [(let ([,uvar* ,[(Expr #t) -> expr*]] ...) ,[sub_expr])
                    `(let ([,uvar* ,expr*] ...) ,sub_expr)]
                [(letrec ([,uvar* ,[(Expr #t) -> expr*]] ...) ,[sub_expr])
                    `(letrec ([,uvar* ,expr*] ...) ,sub_expr)]
                [(,[expr*] ...) `(,@expr*)]
                [,x x])))
    ((Expr #f) program))
