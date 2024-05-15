(define (expose-memory-operands program)
    (define (handle_memory_location base offset)
        (cond
            [(and (register? base) (register? offset))
                (make-index-opnd base offset)]
            [(register? base)
                (make-disp-opnd base offset)]
            [(register? offset)
                (make-disp-opnd offset base)]
            [else (error "Unable to handle memory location ~s ~s.\n" base offset)]))
    (define (Stat statement)
        (match statement
            [(begin ,[statement*] ...) (make-begin statement*)]
            [(if ,[statement*] ...) `(if ,statement* ...)]
            [(set! ,uvar (mref ,base ,offset))
                `(set! ,uvar ,(handle_memory_location base offset))]
            [(mset! ,base ,offset ,triv)
                `(set! ,(handle_memory_location base offset) ,triv)]
            [(mref ,base ,offset) (handle_memory_location base offset)]
            [,x x]))
    (match program
        [(letrec ([,label* (lambda () ,[Stat -> tail*])] ...) ,[Stat -> tail])
            `(letrec ([,label* (lambda () ,tail*)] ...) ,tail)]))