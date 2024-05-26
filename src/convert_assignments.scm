(define (convert-assignments program)
    ; Return three values
    ;   1. A list of temporary variables to be used in place of assigned variables
    ;   2. An association list mapping assigned variables to temporary variables
    ;   3. A list of binded_uvars with assigned variables replaced by temporary variables
    (define (replace_assigned_vars assigned_vars binded_uvars)
        (let*
            ([temp_uvars (map (lambda (x) (unique-name 'tmp)) assigned_vars)]
            [assoc_list (map cons assigned_vars temp_uvars)]
            [new_uvars 
                (map (lambda (x) 
                        (if (assoc x assoc_list) 
                            (cdr (assoc x assoc_list)) 
                            x))
                    binded_uvars)])
            (values temp_uvars assoc_list new_uvars)))
    (define (handle_assigned assigned_vars temp_uvars)
        (map (lambda (x t)
                `(x (cons ,t (void))))
            assigned_vars temp_uvars))
    (define (Expr assigned_vars)
        (lambda (program)
            (match program
                [(lambda ,uvar* 
                    (assigned ,assigned_var*
                        ,[(Expr (union assigned_vars assigned_var*)) -> body]))
                    (let-values
                        ([(temp_uvars assoc_list new_uvar*) (replace_assigned_vars assigned_var* uvar*)])
                        `(lambda ,uvar*
                            (let ,(handle_assigned assigned_var* temp_uvars)
                                ,body)))]
                [(,let_form ([,uvar* ,bind_expr*] ...)
                    (assigned ,assigned_var*
                        ,[(Expr (union assigned_vars assigned_var*)) -> body])) (guard (eq? let_form 'let) (eq? let_form 'letrec))
                    (let-values
                        ([(temp_uvars assoc_list new_uvar*) (replace_assigned_vars assigned_var* uvar*)])
                        (let ([bind_expr* (map (Expr (union assigned_vars assigned_var*)) bind_expr*)])
                            `(,let_form ([,new_uvar* ,bind_expr*] ...)
                                (let ,(handle_assigned assigned_var* temp_uvars)
                                    ,body))))]
                [(set! ,uvar ,[expr]) 
                    (if (member uvar assigned_vars)
                        `(set-car! ,uvar ,expr)
                        `(set! ,uvar ,expr))]
                [(,[expr*] ...) `(,@expr*)]
                [,x (if (member x assigned_vars) `(car ,x) x)])))
    ((Expr '()) program))