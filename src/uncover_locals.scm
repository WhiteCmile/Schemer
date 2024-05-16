(define (uncover-locals program)
    (define (Body tail)
        (let ([uvars (Stat tail)])
            `(locals ,uvars ,tail)))
    ; Stat handles Tail, Pred, Effect and Value
    (define (Stat statement)
        (match Stat
            [(begin ,[Stat -> uvar**] ...) (apply append uvar**)]
            [(if ,[Stat -> uvar**] ...) (apply append uvar**)]
            [(let ,binding* ,[Stat -> uvar*]) (append (map car binding*) uvar*)]
            [(,[Stat -> uvar**] ...) (apply append uvar**)]
            [,x '()]))
    (match program
        [(letrec ([,label* (lambda ,uvar** ,[Body -> body*])] ...) ,[Body -> body])
            `(letrec ([,label* (lambda ,uvar** ,body*)] ...) ,body)]))