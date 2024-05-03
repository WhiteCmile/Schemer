(load "lib/match.scm")

(define flatten-program
    (lambda (program)
        ; flatten label* and tail*
        (define flatten_lbl_tail
            (lambda (label* tail*)
                (match label*
                    [(,label ,labels* ...) (append (cons label (car tail*)) (flatten_lbl_tail (cdr label*) (cdr tail*)))]
                    [,x '()])))
        (define flatten_statement
            (lambda (statement)
                (match statement
                    [(begin ,[flatten_statement -> effect*] ... ,[flatten_statement -> tail])
                        (append (apply append effect*) tail)]
                    [(,triv) `((jump ,triv))]
                    [,x `(,x)])))
        (match program
            [(letrec ([,label* (lambda () ,[flatten_statement -> tail*])] ...) ,[flatten_statement -> body_tail])
                (cons 'code (append body_tail (flatten_lbl_tail label* tail*)))])))