(define (convert-complex-datum program)
    ; All helpers return four values:
    ;   1. New variables defined to bind complex datum
    ;   2. Complex datum values
    ;   3. Set expressions to initialize vector datum
    ;   4. Program with complex datum replaced by new variables

    ; All vector will be transformed into a new variable
    ; while all pairs will be the form of (cons car cdr)
    (define (handle_quoted_data data)
        ; Integrate all sub expressions to make a new data_expr
        (define (integrate_sub_exprs data_expr new_vars new_values set_exprs)
            (if (null? new_vars)
                data_expr
                `(let ,(map list new_vars new_values)
                    ,(make-begin `(,@set_exprs ,data_expr)))))
        ; Transform the pair into (cons car cdr) form
        (define (handle_pair pair)
            (match pair
                [,x (guard (null? x)) 
                    (values '() '() '() ''())]
                [,x (guard (not (pair? x))) 
                    (handle_quoted_data x)]
                [,pair 
                    (let-values 
                        ([(car_new_uvars car_values car_set_exprs car_expr) (handle_pair (car pair))]
                        [(cdr_new_uvars cdr_values cdr_set_exprs cdr_expr) (handle_pair (cdr pair))])
                        (values '() '() '()
                            (integrate_sub_exprs 
                                `(cons ,car_expr ,cdr_expr)
                                (append car_new_uvars cdr_new_uvars)
                                (append car_values cdr_values)
                                (append car_set_exprs cdr_set_exprs))))]))
        ; Get the expressions to initialize vector datum
        (define (set_vector vec vec_name)
            (let loop ([vec vec] [i 0])
                (match vec
                    [() '()]
                    [(,elem . ,rest)
                        (let-values
                            ([(elem_new_vars elem_values elem_set_exprs elem_expr) 
                                (handle_quoted_data elem)])
                            (cons 
                                `(vector-set! ,vec_name (quote ,i) 
                                    ,(integrate_sub_exprs
                                        elem_expr
                                        elem_new_vars
                                        elem_values
                                        elem_set_exprs))
                                (loop rest (add1 i))))])))
        (define (handle_vector vec)
            (let ([new_var (unique-name 'vec)])
                (let
                    ([set_exprs (set_vector vec new_var)])
                    (values 
                        `(,new_var) 
                        `((make-vector (quote ,(length vec))))
                        set_exprs
                        new_var))))
        (match data
            [#(,elem* ...) (handle_vector elem*)]
            [,pair (guard (pair? pair)) (handle_pair pair)]
            [,x (values '() '() '() `(quote ,x))]))
    ; Handle expressions
    (define (Expr expr)
        (match expr
            [(quote ,data) 
                (let-values 
                    ([(new_vars datum_values set_exprs data_expr) (handle_quoted_data data)])
                    ; If the returned data is a pair, we need to bind it to a new variable
                    (if (pair? data)
                        (let ([new_var (unique-name 'cmplx_datum)])
                            (values `(,new_var) `(,data_expr) '() new_var))
                        (values new_vars datum_values set_exprs data_expr)))]
            [(,[Expr -> new_vars* values* set_exprs* expr*] ...)
                (values (apply append new_vars*)
                        (apply append values*)
                        (apply append set_exprs*)
                        `(,@expr*))]
            [,x (values '() '() '() x)]))
    (let-values
        ([(new_vars datum_values set_exprs expr) (Expr program)])
        `(let ,(map list new_vars datum_values)
            ,(make-begin `(,@set_exprs ,expr)))))
