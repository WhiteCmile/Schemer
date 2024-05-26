(define (uncover-assigned program)
    ; Return two values
    ; 1. The set of variables assigned in the program
    ; 2. Transformed program after collecting all assigned variables
    (define (Expr expr)
        (match expr
            [(lambda ,uvars ,[Expr -> assigned_vars lambda_body])
                (values (difference assigned_vars uvars)
                    `(lambda ,uvars
                        (assigned ,(intersection assigned_vars uvars)
                            ,lambda_body)))]
            [(,let_form 
                ([,uvar* ,[Expr -> assigned_vars* bind_expr*]] ...)
                ,[Expr -> assigned_vars body_expr])
                (guard (or (eq? let_form 'let) (eq? let_form 'letrec))
                    (let ([assigned_vars (union assigned_vars (apply union assigned_vars*))])
                        (values (difference assigned_vars uvar*)
                            `(,let_form ([,uvar* ,bind_expr*] ...)
                                (assigned ,(intersection assigned_vars uvar*)
                                    ,body_expr)))))]
            [(set! ,uvar ,[Expr -> assigned_vars expr])
                (values (union assigned_vars `(,uvar))
                    `(set! ,uvar ,expr))]
            [(,[Expr -> assigned_vars* expr*] ...)
                (values (apply union assigned_vars*) `(,@expr*))]
            [,x (values '() x)]))
    (let-values
        ([(assigned_vars expr) (Expr program)])
        expr))