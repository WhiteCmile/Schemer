(define (lift-letrec program)
    (define (Expr expr)
        (match expr
            [(let 
                ([,uvar* ,[Expr -> label** lambda** expr*]] ...)
                ,[Expr -> label* lambda* sub_expr])
                (values 
                    `(,@(apply append label**) ,@label*)
                    `(,@(apply append lambda**) ,@lambda*)
                    `(let ([,uvar* ,expr*] ...) ,sub_expr))]
            [(letrec
                ([,label* (lambda ,uvar** ,[Expr -> label** lambda** expr*])] ...)
                ,[Expr -> sub_label* sub_lambda* sub_expr])
                (values
                    `(,@label* ,@(apply append label**) ,@sub_label*)
                    `((lambda ,uvar** ,expr*) ... ,@(apply append lambda**) ,@sub_lambda*)
                    sub_expr)]
            [(,[Expr -> label** lambda** expr*] ...)
                (values
                    (apply append label**)
                    (apply append lambda**)
                    `(,@expr*))]
            [,x (values '() '() x)])) 
    (let-values 
        ([(labels lambdas expr) (Expr program)])
        `(letrec
            ,(let loop 
                ([labels labels] 
                [lambdas lambdas])
                (match labels
                    [() '()]
                    [(,label . ,rest)
                        (cons `(,label ,(car lambdas)) (loop rest (cdr lambdas)))]))
            ,expr)))
