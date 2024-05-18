(define (make-nopless-begin x*)
    (let ([x* (remove '(nop) x*)])
        (if (null? x*)
            '(nop)
            (make-begin x*))))

(define (normalize-context program)
    ; This helper handles expressions of begin, if and let
    ; Just for convenience, we will use the same function for all three
    (define (Stmt Form)
        (lambda (statement)
            (match statement
                [(begin ,[Effect -> effect*] ... ,[Form -> form])
                    (make-nopless-begin `(,@effect* ,form))]
                [(if ,[Pred -> pred] ,[Form -> form*] ...)
                    `(if ,pred ,@form*)]
                [(let ([,uvar* ,[Value -> value*]] ...) ,[Form -> form])
                    `(let ([,uvar* ,value*] ...) ,form)])))
    (define (Value statement)
        (match statement
            [(quote ,immediate) statement]
            [,label (guard (label? label)) label]
            [,uvar (guard (uvar? uvar)) uvar]
            [(,prim ,[Value -> value*] ...) (guard (prim? prim))
                (cond
                    [(value-prim? prim) `(,prim ,@value*)]
                    [(effect-prim? prim) (make-nopless-begin `((,prim ,@value*) (void)))]
                    [else `(if (,prim ,@value*) '#t '#f)])]
            [(,proc ,[Value -> value*] ...) (guard (not (keyword? proc)))
                `(,(Value proc) ,@value*)]
            [,x ((Stmt Value) x)]))
    (define (Effect statement)
        (match statement
            [(quote ,immediate) '(nop)]
            [,label (guard (label? label)) '(nop)]
            [,uvar (guard (uvar? uvar)) '(nop)]
            [(,prim ,[Value -> value*] ...) (guard (prim? prim))
                (cond
                    [(effect-prim? prim) `(,prim ,@value*)]
                    [else (make-nopless-begin (map Effect value*))])]
            [(,proc ,[Value -> value*] ...) (guard (not (keyword? proc)))
                `(,(Value proc) ,@value*)]
            [,x ((Stmt Effect) x)]))
    (define (Pred statement)
        (define (value_to_pred value)
            `(if (eq? ,value '#f) (false) (true)))
        (match statement
            [(quote ,immediate) 
                (if (eq? immediate #f)
                    '(false)
                    '(true))]
            [,label (guard (label? label)) (value_to_pred label)]
            [,uvar (guard (uvar? uvar)) (value_to_pred uvar)]
            [(,prim ,[Value -> value*] ...) (guard (prim? prim))
                (cond
                    [(pred-prim? prim) `(,prim ,@value*)]
                    [(value-prim? prim) (value_to_pred `(,prim ,@value*))]
                    [else (make-nopless-begin `((,prim ,@value*) (true)))])]
            [(,proc ,[Value -> value*] ...) (guard (not (keyword? proc)))
                (value_to_pred `(,(Value proc) ,@value*))]
            [,x ((Stmt Pred) x)]))
    (match program
        [(letrec ([,label* (lambda ,uvar** ,[Value -> expr*])] ...) ,[Value -> expr])
            `(letrec ([,label* (lambda ,uvar** ,expr*)] ...)
                ,expr)]))