(load "lib/match.scm")

(define discard-call-live
    (lambda (program)
        (define Body
            (lambda (body)
                (match body
                    [(locate ,bind_list ,[Tail -> tail])
                        `(locate ,bind_list ,tail)])))
        (define Tail
            (lambda (tail)
                (match tail
                    [(begin ,effect* ... ,[Tail -> sub_tail])
                        `(begin ,effect* ... ,sub_tail)]
                    [(if ,pred ,[Tail -> tail1] ,[Tail -> tail2])
                        `(if ,pred ,tail1 ,tail2)]
                    [(,triv ,Loc* ...) `(,triv)])))
        (match program
            [(letrec ([,label* (lambda () ,[Body -> body*])] ...) ,[Body -> body])
                `(letrec ([,label* (lambda () ,body*)] ...) ,body)])))