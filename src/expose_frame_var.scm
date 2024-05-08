(load "lib/match.scm")

(define expose-frame-var
    (letrec 
        ([handle_statement 
            (lambda (statement)
                (match statement
                    [(begin ,[handle_statement -> effect*] ... ,[handle_statement -> tail]) `(begin ,effect* ... ,tail)]
                    [(if ,[handle_statement -> stat1] ,[handle_statement -> stat2] ,[handle_statement -> stat3])
                        `(if ,stat1 ,stat2 ,stat3)]
                    [(set! ,[handle_var -> var1] (,binop ,[handle_triv -> triv1] ,[handle_triv -> triv2])) 
                        `(set! ,var1 (,binop ,triv1 ,triv2))]
                    [(set! ,[handle_var -> var] ,[handle_triv -> triv]) `(set! ,var ,triv)]
                    [(,relop ,[handle_triv -> triv1] ,[handle_triv -> triv2])
                        `(,relop ,triv1 ,triv2)]
                    [(,triv) (guard (triv? triv)) (list (handle_triv triv))]
                    [,x x]))]
        [handle_var
            (lambda (var)
                (if (register? var)
                    var
                    (make-disp-opnd 'rbp (* 8 (frame-var->index var)))))]
        [handle_triv
            (lambda (triv)
                (match triv
                    [,x (guard (int64? x)) x]
                    [,x (guard (label? x)) x]
                    [,x (handle_var x)]))])
        (lambda (program)
            (match program
                [(letrec ([,label* (lambda () ,[handle_statement -> tail*])] ...) ,[handle_statement -> body_tail])
                    `(letrec ([,label* (lambda () ,tail*)] ...) ,body_tail)]))))