(define remove-complex-opera*
    (lambda (program)
        ; This helper handles Tail, Pred, Effect structure
        (define Stat
            (lambda (statement)
                (match statement
                    [(begin ,[uvar** statement*] ...)
                        (values (apply append uvar**)
                                (make-begin statement*))]
                    [(if ,[uvar** statement*] ...)
                        (values (apply append uvar**)
                                `(if ,statement* ...))]
                    [(set! ,var ,[Value -> uvars new_value])
                        (values uvars
                                `(set! ,var ,new_value))]
                    [(,[value_expression -> header* uvar** value*] ...)
                        (values (apply append uvar**)
                            (make-begin (append (apply append header*) `((,value* ...)))))]
                    [,x (values '() x)])))
        ; Value and value_expression are two helpers that do the same thing
        ; except that value_expression returns three values: header, uvars, and value
        ; but Value returns two values: uvars and Value, while value is (make-begin (append header `(,value)))
        ; For convenience, an operator may be used to create a Value
        ; In this case, we just return it with no header and uvars
        (define Value
            (lambda (value)
                (let-values ([(header uvars new_value) (value_expression value)])
                    (values uvars
                            (make-begin (append header `(,new_value)))))))
        ; header is a list of expressions
        ; and the returned value must be a triv
        (define value_expression
            (lambda (value)
                (match value
                    [,triv (guard (triv? triv))
                        (values '() '() triv)]
                    [(begin ,[Stat -> uvar** effect*] ... ,[value_expression -> header uvars sub_value])
                        (let* ([new_var (unique-name 'value)])
                            (values (append (append effect* header) `((set! ,new_var ,sub_value)))
                                    (cons new_var (append (apply append uvar**) uvars))
                                    new_var))]
                    [(if ,[Stat -> pred_uvar pred] ,[Value -> uvar** value*] ...)
                        (let*
                            ([new_var (unique-name 'value)]
                            [uvars (cons new_var (append pred_uvar (apply append uvar**)))])
                            (values (list `(set! ,new_var (if ,pred ,value* ...)))
                                    uvars
                                    new_var))]
                    [(,binop ,[value_expression -> header* uvar** value*] ...)
                        (let*
                            ([new_var (unique-name 'value)]
                            [header (append (apply append header*) `((set! ,new_var (,binop ,value* ...))))]
                            [uvars (cons new_var (apply append uvar**))])
                            (values header
                                    uvars
                                    new_var))]
                    [,x (values '() '() x)])))
        (match program
            [(letrec ([,label* (lambda ,uvar** ,[body*])] ...) ,[body])
                `(letrec ([,label* (lambda ,uvar** ,body*)] ...) ,body)]
            [(locals ,uvar* ,[Stat -> new_uvars tail])
                `(locals ,(append uvar* new_uvars) ,tail)])))