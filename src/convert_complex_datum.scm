(define (convert-complex-datum program)
    ; All helpers return four values:
    ;   1. New variables defined to bind complex datum
    ;   2. Complex datum values
    ;   3. Set expressions to initialize vector datum
    ;   4. Program with complex datum replaced by new variables
    (define (handle_quoted_data data)
        ; Transform the pair into (cons car cdr) form
        (define (flatten_pair pair)
            (match pair
                [,x (guard (null? x)) 
                    (values '() '() '() ''())]
                [,x (guard (not (pair? x))) 
                    (handle_quoted_data x)]
                [,pair 
                    (let-values 
                        ([(car_new_vars car_values car_set_exprs car_expr) (flatten_pair (car pair))]
                        [(cdr_new_vars cdr_values cdr_set_exprs cdr_expr) (flatten_pair (cdr pair))])
                        (let ([new_vars (append car_new_vars cdr_new_vars)]
                            [new_values (append car_values cdr_values)]
                            [new_set_exprs (append car_set_exprs cdr_set_exprs)])
                            (values '() '() '() 
                                (if (null? new_vars)
                                    `(cons ,car_expr ,cdr_expr)
                                    `(let ,(map list new_vars new_values)
                                        ,(make-begin 
                                            `(,@new_set_exprs (cons ,car_expr ,cdr_expr))))))))]))
        (define (set_vector vec vec_name)
            (let loop ([vec vec] [i 0])
                (match vec
                    [() (values '() '() '() '())]
                    [(,elem . ,rest)
                        (let-values
                            ([(rest_new_vars rest_values rest_set_exprs sub_set_exprs) 
                                (loop rest (add1 i))]
                            [(elem_new_vars elem_values elem_set_exprs elem_expr) 
                                (handle_quoted_data elem)])
                            (values
                                (append elem_new_vars rest_new_vars)
                                (append elem_values rest_values)
                                (cons `(vector-set! ,vec_name (quote ,i) ,elem_expr) rest_set_exprs)
                                (append elem_set_exprs sub_set_exprs)))])))
        (define (handle_vector vec)
            (let ([new_var (unique-name 'vec)])
                (let-values 
                    ([(new_vars datum_values cur_set_exprs sub_set_exprs) (set_vector vec new_var)])
                    (values `(,new_var)
                            `((make-vector (quote ,(length vec))))
                            cur_set_exprs
                            (if (null? new_vars)
                                new_var
                                `(let ,(map list new_vars datum_values)
                                    ,(make-begin `(,@sub_set_exprs new_var))))))))
        (match data
            [#(,elem* ...) (handle_vector elem*)]
            [,pair (guard (pair? pair)) (flatten_pair pair)]
            [,x (values '() '() '() `(quote ,x))]))
    ; Handle expressions
    (define (Expr expr)
        (match expr
            [(quote ,data) 
                (let-values 
                    ([(new_vars datum_values set_exprs data_expr) (handle_quoted_data data)])
                    (cond 
                        [(pair? data) 
                            (let ([new_var (unique-name 'pair)])
                                (values (append new_vars `(,new_var))
                                        (append datum_values `(,data_expr))
                                        set_exprs
                                        new_var))]
                        [else (values new_vars datum_values set_exprs data_expr)]))]
            [(,[Expr -> new_vars* values* set_exprs* expr*] ...)
                (values (apply append new_vars*)
                        (apply append values*)
                        (apply append set_exprs*)
                        `(,@expr*))]
            [,x (values '() '() '() x)]))
    (let-values
        ([(new_vars datum_values set_exprs expr) (Expr program)])
        `(let ,(map list new_vars datum_values)
            ,(make-begin (append set_exprs `(,expr))))))
