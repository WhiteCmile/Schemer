(define (optimize-direct-call program)
    (define (Expr expr)
        (match expr
            [(begin ,[expr*] ...) `(begin ,@expr*)]
            [(if ,[expr*] ...) `(if ,@expr*)]
            [(let ([,uvar* ,[expr*]] ...) ,[sub_expr])
                `(let ([,uvar* ,expr*] ...) ,sub_expr)]
            [(letrec ([,uvar* ,[lambda_expr*]] ...) ,[sub_expr])
                `(letrec ([,uvar* ,lambda_expr*] ...) ,sub_expr)]
            [(lambda ,uvar* ,[expr]) `(lambda ,uvar* ,expr)]
            [((lambda ,param* ,[lambda_expr]) ,arg* ...)
                (if (eq? (length param*) (length arg*))
                    `(let ([,param* ,arg*] ...) ,lambda_expr)
                    `((lambda ,param* ,lambda_expr) ,@arg*))]
            [(,[expr*] ...) `(,@expr*)]
            [,x x]))
    (Expr program))