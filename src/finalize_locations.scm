(load "lib/match.scm")
(load "lib/my_helpers.scm")

(define substitute
    (lambda (uvars Locs x)
        (match x
            [,x (guard (eq? (car uvars) x)) (car Locs)]
            [,x (substitute (cdr uvars) (cdr Locs) x)])))

(define finalize-locations
    (lambda (program)
        (define Body
            (lambda (body)
                (match body
                    [(locate ([,uvar* ,Loc*] ...) ,tail)
                        ((Stat_with_2lists uvar* Loc*) tail)])))
        (define Stat_with_2lists
            (lambda (uvars Locs)
                (lambda (statement)
                    (let ([Stat (Stat_with_2lists uvars Locs)]
                            [Var (Var_with_2lists uvars Locs)]
                            [Triv (Triv_with_2lists uvars Locs)])
                        (match statement
                            [(begin ,[Stat -> effect*] ... ,[Stat -> stat]) 
                                `(begin ,effect* ... ,stat)]
                            [(if ,[Stat -> stat1] ,[Stat -> stat2] ,[Stat -> stat3])
                                `(if ,stat1 ,stat2 ,stat3)]
                            [(set! ,[Var -> var] (,binop ,[Triv -> triv1] ,[Triv -> triv2]))
                                `(set! ,var (,binop ,triv1 ,triv2))]
                            [(set! ,[Var -> var] ,[Triv -> triv])
                                `(set! ,var ,triv)]
                            [(,relop ,[Triv -> triv1] ,[Triv -> triv2])
                                `(,relop ,triv1 ,triv2)]
                            [(,triv) (guard (triv? triv)) (list (Triv triv))]
                            [,x x])))))
        (define Var_with_2lists
            (lambda (uvars Locs)
                (lambda (var)
                    (match var
                        [,x (guard (or (register? x) (frame-var? x))) x]
                        [,x (substitute uvars Locs x)]))))
        (define Triv_with_2lists
            (lambda (uvars Locs)
                (lambda (triv)
                    (match triv
                        [,x (guard (or (int64? x) (label? x))) x]
                        [,x ((Var_with_2lists uvars Locs) x)]))))
        ; Label is used to reset unique-name-count
        (define Label
            (lambda (label)
                (let ([num (string->number (extract-suffix label))])
                    (if (> num (unique-name-count))
                        (begin 
                            (unique-name-count num)
                            label)
                        label))))
        (match program
            [(letrec ([,[Label -> label*] (lambda () ,[Body -> body*])] ...) ,[Body -> main_body])
                `(letrec ([,label* (lambda () ,body*)] ...) ,main_body)])))

(define finalize-frame-locations
    (lambda (program)
        (define Stat
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
                        [(,triv) (guard (triv? triv)) (replace triv)]
                        [,x x]))))
        (match program
            [(letrec ([,label* (lambda () ,[body*])] ...) ,[body])
                `(letrec ([,label* (lambda () ,body*)] ...) ,body)]
            [(locals ,uvar*
                (ulocals ,uloc*
                    (locate ,binding*
                        (frame-conflict ,graph ,[(Stat binding*) -> tail]))))
                `(locals ,uvar*
                    (ulocals ,uloc*
                        (locate ,binding*
                            (frame-conflict ,graph ,tail))))]
            [,x x])))