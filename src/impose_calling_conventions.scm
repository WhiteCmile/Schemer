; A helper that assign registers to parameters
; It returns three values (rest_params used_regs set_to_regs)
;   rest_params: the rest of parameters that are not assigned to registers
;   used_regs: the registers that are used to assign to parameters
;   set_to_regs: the set! statements that assign parameters to registers
(define (assign_regs_to_params params)
    (let loop
        ([params params]
        [regs parameter-registers])
        (match params
            [() (values '() regs '())]
            [(,var . ,rest)
                (if (null? regs)
                    (values params '() '())
                    (let-values 
                        ([(rest_params used_regs set_to_regs) (loop rest (cdr regs))])
                        (values 
                            rest_params 
                            (cons (car regs) used_regs) 
                            (cons `(set! ,(car regs) ,var) set_to_regs))))])))

(define (impose-calling-conventions program)
    (define fp frame-pointer-register)
    (define ra return-address-register)
    (define rv return-value-register)
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
            ([(rest_params used_regs set_of_regs) (assign_regs_to_params params)])
            (make-begin 
                (let loop
                    ([params rest_params]
                    [pos 0]
                    [assigned_fvs '()])
                    (match params
                        [() (append set_of_regs `((set! ,ra ,rp) (,proc ,fp ,ra ,@used_regs ,@assigned_fvs)))]
                        [(,var . ,rest)
                            (let* 
                                ([fv (index->frame-var pos)]
                                [statements (loop rest (add1 pos) (cons fv assigned_fvs))])
                                (cons `(set! ,fv ,var) statements))])))))
    ; Handle non-tail calls, assign appropriate registers or new frame locations to actual parameters
    (define (nontail_call proc params)
        (define rp-label (unique-label 'rp-label))
        `(return-point ,rp-label
            ,(make-begin 
                (let-values
                    ([(rest_params used_regs set_of_regs) (assign_regs_to_params params)])
                    (let loop ([params rest_params] [assigned_nfvs '()])
                        (match params
                            [() `(,@set_of_regs (set! ,ra ,rp-label) (,proc ,fp ,ra ,@used_regs ,@assigned_nfvs))]
                            [(,var . ,rest)
                                (let ([nfv (unique-name 'nfv)])
                                    `((set! ,nfv ,var) ,@(loop rest (cons nfv assigned_nfvs))))]))))))
    (define (Body_with_wrapper body_with_lambda)
        (define (Body uvars body)
            (define new_frame_lists '())
            (define (Tail rp tail)
                (match tail
                    [(begin ,[Stat -> effect*] ... ,sub_tail) 
                        (make-begin (append effect* (list (Tail rp sub_tail))))]
                    [(if ,[Stat -> pred] ,tail1 ,tail2)
                        `(if ,pred ,(Tail rp tail1) ,(Tail rp tail2))]
                    [(,triv ,triv* ...) (guard (not (binop? triv)))
                        (handle_actual_params rp triv triv*)]
                    [,expr 
                        `(begin 
                            (set! ,rv ,expr) 
                            (,rp ,fp ,rv))]))
            ; Stat helper handles Pred and Effect structures
            (define (Stat statement)
                (match statement
                    [(begin ,[Stat -> statement*] ...) `(begin ,statement* ...)]
                    [(if ,[Stat -> statement*] ...) `(if ,statement* ...)]
                    [(,triv ,triv* ...) (guard (triv? triv))
                        (nontail_call triv triv*)]
                    [(set! ,uvar (,triv ,triv* ...)) (guard (triv? triv))
                        (make-begin 
                            `(,(Stat `(,triv ,triv* ...))
                                (set! ,uvar ,rv)))]
                    [,x x]))
            (match body
                [(locals ,body_uvar* ,tail)
                    (let ([rp (unique-name 'rp)])
                        `(locals `(,@body_uvar* ,@(cons rp uvars) ,@(apply union new_frame_lists))
                            (new-frames ,new_frame_lists
                                ,(make-begin 
                                    `(,(handle_formal_params uvars rp)
                                        ,(Tail rp tail))))))]))
        (match body_with_lambda
            [(lambda ,uvar* ,body) (Body uvar* body)]))
    (match program
        [(letrec ([,label* ,[Body_with_wrapper -> body*]] ...) ,body)
            `(letrec ([,label* (lambda () ,body*)] ...) ,(Body_with_wrapper `(lambda () ,body)))]))