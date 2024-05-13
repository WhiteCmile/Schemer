(define (impose-calling-conventions program)
    (define fp frame-pointer-register)
    (define ra return-address-register)
    (define rv return-value-register)
    (define (Body_with_wrapper body_with_lambda)
        (define (Body uvars body)
            (match body
                [(locals ,body_uvar* ,tail)
                    (let ([rp (unique-name 'rp)])
                        `(locals ,(append body_uvar* (cons rp uvars))
                            ,(make-begin 
                                `(,(handle_formal_params uvars rp)
                                    ,(Tail rp tail)))))]))
        (match body_with_lambda
            [(lambda ,uvar* ,body) (Body uvar* body)]))
    ; Initialize the formal parameters of corresponding registers and frame locations by calling convention
    (define (handle_formal_params params rp)
        (make-begin 
            (cons `(set! ,rp ,ra)
                (let loop 
                    ([params params] 
                    [regs parameter-registers] 
                    [pos 0])
                    (match params
                        [() '()]
                        [(,uvar . ,rest)
                            (if (null? regs)
                                (cons `(set! ,uvar ,(index->frame-var pos))
                                    (loop rest regs (add1 pos)))
                                (cons `(set! ,uvar ,(car regs))
                                    (loop rest (cdr regs) pos)))])))))
    ; Assign appropriate registers or frame locations to actual parameters
    (define (handle_actual_params rp proc params)
        (let-values
            ([(rest_params rest_regs set_of_regs)
                (let loop
                    ([params params]
                    [regs parameter-registers])
                    (match params
                        [() (values '() regs '())]
                        [(,var . ,rest)
                            (if (null? regs)
                                (values params '() '())
                                (let-values 
                                    ([(rest_params rest_regs set_to_regs) (loop rest (cdr regs))])
                                    (values rest_params rest_regs (cons `(set! ,(car regs) ,var) set_to_regs))))]))])
            (make-begin 
                (let loop
                    ([params rest_params]
                    [pos 0]
                    [assigned_fvs '()])
                    (match params
                        [() (append set_of_regs `((,proc ,fp ,ra ,@rest_regs ,@assigned_fvs)))]
                        [(,var . ,rest)
                            (let* 
                                ([fv (index->frame-var pos)]
                                [statements (loop rest (add1 pos) (cons fv assigned_fvs))])
                                (cons `(set! ,fv ,var) statements))])))))
    (define (Tail rp tail)
        (match tail
            [(begin ,effect* ... ,sub_tail) 
                (make-begin (append effect* (list (Tail rp sub_tail))))]
            [(if ,pred ,tail1 ,tail2)
                `(if ,pred ,(Tail rp tail1) ,(Tail rp tail2))]
            [(,triv ,triv* ...) (guard (not (binop? triv)))
                (handle_actual_params rp triv triv*)]
            [,expr 
                `(begin 
                    (set! ,rv ,expr) 
                    (,rp ,fp ,rv))]))
    (match program
        [(letrec ([,label* ,[Body_with_wrapper -> body*]] ...) ,body)
            `(letrec ([,label* (lambda () ,body*)] ...) ,(Body_with_wrapper `(lambda () ,body)))]))