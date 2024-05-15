(load "lib/match.scm")

(define (expose-frame-var program)
    (define fp frame-pointer-register)
    (define (Body tail)
        (define rbp_offset 0)
        (define (Stats statements)
            (match statements
                [() '()]
                [(,statement . ,rest)
                    (let* 
                        ([statement (Stat statement)]
                        [rest (Stats rest)])
                    (cons statement rest))]))
        (define (Stat statement)
            (match statement
                [(begin ,stat* ...) 
                    (let ([stats (Stats stat*)]) 
                        `(begin ,@stats))]
                [(if ,stat* ...) 
                    (let ([stats (Stats stat*)])
                        `(if ,@stats))]
                [(set! ,[Var -> var1] (,binop ,[Triv -> triv1] ,[Triv -> triv2])) 
                    (if (eq? var1 fp)
                        (match binop
                            [+ (set! rbp_offset (+ rbp_offset triv2))]
                            [- (set! rbp_offset (- rbp_offset triv2))]))
                    `(set! ,var1 (,binop ,triv1 ,triv2))]
                [(set! ,[Var -> var] ,[Triv -> triv]) `(set! ,var ,triv)]
                [(return-point ,label ,[Stat -> tail])
                    `(return-point ,label ,tail)]
                [(,relop ,[Triv -> triv1] ,[Triv -> triv2])
                    (guard (relop? relop))
                    `(,relop ,triv1 ,triv2)]
                [(,triv) (guard (triv? triv)) (list (Triv triv))]
                [,x x]))
        (define (Var var)
            (if (frame-var? var)
                (make-disp-opnd fp (- (ash (frame-var->index var) align-shift) rbp_offset))
                var))
        (define (Triv triv)
            (match triv
                [,x (guard (int64? x)) x]
                [,x (guard (label? x)) x]
                [,x (Var x)]))
        (match tail
            [,[Stat -> tail] tail]))
    (match program
        [(letrec ([,label* (lambda () ,[Body -> tail*])] ...) ,[Body -> body_tail])
            `(letrec ([,label* (lambda () ,tail*)] ...) ,body_tail)]))