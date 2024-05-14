(load "lib/match.scm")

(define discard-call-live
    (lambda (program)
        (define Body
            (lambda (body)
                (match body
                    [(locate ,bind_list ,[Stat -> tail])
                        `(locate ,bind_list ,tail)])))
        (define Stat
            (lambda (statement)
                (match statement
                    [(begin ,[statement*] ...) `(begin ,statement* ...)]
                    [(if ,[statement*] ...) `(if ,statement* ...)]
                    [(return-point ,label ,[tail]) `(return-point ,label ,tail)]
                    [(,triv ,Loc* ...) (guard (triv? triv)) `(,triv)]
                    [,x x])))
        (match program
            [(letrec ([,label* (lambda () ,[Body -> body*])] ...) ,[Body -> body])
                `(letrec ([,label* (lambda () ,body*)] ...) ,body)])))