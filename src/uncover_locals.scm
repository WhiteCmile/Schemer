(define (uncover-locals program)
    (define (Body tail)
        `(locals ,(Stat tail) ,tail))
    ; Stat handles Tail, Pred, Effect and Value
    (define (Stat statement)
        (match statement
            [(begin ,[Stat -> uvar**] ...) (apply append uvar**)]
            [(if ,[Stat -> uvar**] ...) (apply append uvar**)]
            [(let ([,var* ,[Stat -> uvar**]] ...) ,[Stat -> uvar*]) 
                `(,@var* ,@(apply append uvar**) ,@uvar*)]
            [(,[Stat -> uvar**] ...) (apply append uvar**)]
            [,x '()]))
    (match program
        [(letrec ([,label* (lambda ,uvar** ,[Body -> body*])] ...) ,[Body -> body])
            `(letrec ([,label* (lambda ,uvar** ,body*)] ...) ,body)]))