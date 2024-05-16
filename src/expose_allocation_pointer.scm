(define (expose-allocation-pointer program)
    (define ap allocation-pointer-register)
    (define (Stat statement)
        (match statement
            [(begin ,[Stat -> statement*] ...) (make-begin statement*)]
            [(if ,[Stat -> statement*] ...) `(if ,statement* ...)]
            [(set! ,var (alloc ,expr)) 
                `(begin (set! ,var ,ap)
                        (set! ,ap (+ ,ap ,expr)))]
            [(return-point ,label ,[Stat -> tail])
                `(return-point ,label ,tail)]
            [,x x]))
    (match program
        [(letrec ([,label* (lambda ,uvar** ,[body*])] ...) ,[body])
            `(letrec ([,label* (lambda ,uvar** ,body*)] ...) ,body)]
        [(locals ,uvar* 
            (new-frames ,new_frame_lists
                ,[Stat -> tail]))
            `(locals ,uvar* 
                (new-frames ,new_frame_lists
                    ,tail))]))