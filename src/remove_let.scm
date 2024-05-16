(define (remove-let program)
    ; Stat handle Tail, Effect, Pred and Value structures
    (define (Stat statement)
        (match statement
            [(begin ,[statement*] ...) (make-begin statement*)]
            [(if ,[statement*] ...) `(if ,statement* ...)]
            [(mset! ,[value*] ...) `(mset! ,value* ...)]
            [(alloc ,[value]) `(alloc ,value)]
            [(let ([,var* ,[expr*]] ...) ,[statement])
                (make-begin 
                    `((set! ,var* ,expr*) ... ,statement))]
            [(,[value*] ...) `(,value* ...)]
            [,x x]))
    (match program
        [(letrec ([,label* (lambda ,uvar** ,[body*])] ...) ,[body])
            `(letrec ([,label* (lambda ,uvar** ,body*)] ...) ,body)]
        [(locals ,uvar* ,[Stat -> tail]) `(locals ,uvar* ,tail)]))