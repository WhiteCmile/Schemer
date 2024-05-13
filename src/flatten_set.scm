(define (flatten-set! program)
    (define (Stat statement)
        (match statement
            [(begin ,[effect*] ...) (make-begin effect*)]
            [(if ,[stat*] ...) `(if ,stat* ...)]
            [(set! ,uvar ,value) (flatten_set uvar value)]
            [,x x]))
    (define (flatten_set var value)
        (match value
            [(begin ,effect* ... ,sub_value)
                (make-begin (append effect* (list (flatten_set var sub_value))))]
            [(if ,pred ,value1 ,value2)
                `(if ,pred ,(flatten_set var value1) ,(flatten_set var value2))]
            [,x `(set! ,var ,x)]))
    (match program
        [(letrec ([,label* (lambda ,uvar** ,[body*])] ...) ,[body])
            `(letrec ([,label* (lambda ,uvar** ,body*)] ...) ,body)]
        [(locals ,uvar* ,[Stat -> tail]) `(locals ,uvar* ,tail)]))
    