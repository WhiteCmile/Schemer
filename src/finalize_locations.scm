(load "lib/match.scm")
(load "lib/my_helpers.scm")

(define finalize_statement
    (lambda (bindings)
        (define replace (substitute_with_value bindings))
        (lambda (statement)
            (match statement
                [(begin ,[statements*] ...) `(begin ,statements* ...)]
                [(if ,[pred] ,[stat1] ,[stat2]) `(if ,pred ,stat1 ,stat2)]
                [(,relop ,triv1 ,triv2) (guard (relop? relop))
                    `(,relop ,(replace triv1) ,(replace triv2))]
                [(set! ,[var] (,binop ,[triv1] ,[triv2])) 
                    `(set! ,var (,binop ,triv1 ,triv2))]
                [(set! ,[var] ,[triv])
                    (if (eq? var triv)
                        '(nop)
                        `(set! ,var ,triv))]
                [(,triv) (guard (triv? triv)) `(,(replace triv))]
                [(,proc ,triv* ...) (guard (triv? proc)) `(,proc ,(replace triv*) ...)]
                [(return-point ,label ,[tail]) `(return-point ,label ,tail)]
                [,uvar (guard (uvar? uvar)) (replace uvar)]
                [,x x]))))

(define finalize-locations
    (lambda (program)
        (match program
            [(letrec ([,label* (lambda () ,[body*])] ...) ,[main_body])
                `(letrec ([,label* (lambda () ,body*)] ...) ,main_body)]
            [(locate ,binding* ,tail)
                ((finalize_statement binding*) tail)])))

(define finalize-frame-locations
    (lambda (program)
        (match program
            [(letrec ([,label* (lambda () ,[body*])] ...) ,[body])
                `(letrec ([,label* (lambda () ,body*)] ...) ,body)]
            [(locals ,uvar*
                (ulocals ,uloc*
                    (locate ,binding*
                        (frame-conflict ,graph ,tail))))
                `(locals ,uvar*
                    (ulocals ,uloc*
                        (locate ,binding*
                            (frame-conflict ,graph ,((finalize_statement binding*) tail)))))]
            [,x x])))