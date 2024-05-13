(load "lib/match.scm")
(load "lib/my_helpers.scm")

; These helpers is used to check if the given instruction satisfies machine restrictions
; If so, it returns with an empty list of new unspillable variables and the same instruction
; Otherwise, it returns a list of new unspillable variables and some new instructions
(define jump_instruction_select
    (lambda (instruction)
        (match instruction
            [(,triv ,Loc* ...)
                (if (can_be_reg? triv)
                    (values '() instruction)
                    (let ([uloc (unique-name 'uloc)])
                        (values (list uloc)
                                `(begin (set! ,uloc ,triv) (,uloc ,Loc* ...)))))])))

(define rel_instruction_select
    (lambda (instruction)
        (match instruction
            [(,relop ,triv1 ,triv2)
                (cond
                    [(can_be_reg? triv1) (values '() instruction)]
                    [(can_be_reg? triv2) (values '() `(,(inverse_op relop) ,triv2 ,triv1))]
                    [else
                        (let ([uloc (unique-name 'uloc)])
                            (values (list uloc)
                                    `(begin (set! ,uloc ,triv1) (,relop ,uloc ,triv2))))])])))

(define mov_instruction_select
    (lambda (instruction)
        (define gen_valid_mov
            (lambda (var binop triv1 triv2)
                (let ([uloc (unique-name 'uloc)])
                    (if (and (not (int32? triv2)) (int64? triv2))
                        (let-values 
                            ([(ulocs effect) (mov_instruction_select `(set! ,var (,binop ,triv1 ,uloc)))])
                            (values (cons uloc ulocs)
                                    `(begin
                                        (set! ,uloc ,triv2)
                                        ,effect)))
                        (values (list uloc)
                                `(begin
                                    (set! ,uloc ,triv1)
                                    (set! ,uloc (,binop ,uloc ,triv2))
                                    (set! ,var ,uloc)))))))
        (match instruction
            [(set! ,var (,binop ,triv1 ,triv2))
                (if (can_be_reg? var)
                    (cond
                        [(and
                            (eq? triv1 var)
                            (or (can_be_reg? triv2) (int32? triv2)))
                            (values '() instruction)]
                        [(and
                            (eq? triv2 var)
                            (can_swap? binop)
                            (or (can_be_reg? triv1) (int32? triv1)))
                            (values '() `(set! ,var (,binop ,triv2 ,triv1)))]
                        [else (gen_valid_mov var binop triv1 triv2)])
                    (cond
                        [(eq? '* binop) (gen_valid_mov var binop triv1 triv2)]
                        [(and
                            (eq? triv1 var)
                            (or (can_be_reg? triv2) (int32? triv2)))
                            (values '() instruction)]
                        [(and
                            (eq? triv2 var)
                            (can_swap? binop)
                            (or (can_be_reg? triv1) (int32? triv1)))
                            (values '() `(set! ,var (,binop ,triv2 ,triv1)))]
                        [else (gen_valid_mov var binop triv1 triv2)]))]
            [(set! ,var ,triv)
                (cond
                    [(can_be_reg? var) (values '() instruction)]
                    [(or (int32? triv) (can_be_reg? triv)) (values '() instruction)]
                    [else
                        (let ([uloc (unique-name 'uloc)])
                            (values (list uloc)
                                    `(begin (set! ,uloc ,triv) (set! ,var ,uloc))))])])))

(define select-instructions
    (lambda (program)
        ; This helper handles Tail, Pred, Effect all together
        (define Stat
            (lambda (statement)
                (match statement
                    [(begin ,[Stat -> new_ulocal_list* statement*] ...)
                        (values
                            (apply union new_ulocal_list*)
                            (make-begin statement*))]
                    ; "uloc" represents for "ulocals"
                    [(if ,[Stat -> uloc_pred pred] ,[Stat -> uloc_t1 tail1] ,[Stat -> uloc_t2 tail2])
                        (values
                            (union uloc_pred uloc_t1 uloc_t2)
                            `(if ,pred ,tail1 ,tail2))]
                    [(set! ,var (,binop ,triv1 ,triv2)) (mov_instruction_select statement)]
                    [(set! ,var ,triv) (mov_instruction_select statement)]
                    [(,relop ,triv1 ,triv2) (guard (relop? relop)) (rel_instruction_select statement)]
                    [(,triv ,Loc* ...) (guard (triv? triv)) (jump_instruction_select statement)]
                    [(return-point ,label ,[Stat -> uloc_list tail])
                        (values uloc_list 
                            `(return-point ,label ,tail))]
                    [,x (values '() x)])))
        (match program
            [(letrec ([,label* (lambda () ,[body*])] ...) ,[body])
                `(letrec ([,label* (lambda () ,body*)] ...) ,body)]
            ; Below statements handle with Body structure
            ; This is just for convenience
            [(locate ,completed* ...) `(locate ,completed* ...)]
            [(locals ,locals
                (ulocals ,ulocals
                    (locate ,bindings
                        (frame-conflict ,conf_graph ,[Stat -> new_ulocals tail]))))
                `(locals ,locals
                    (ulocals ,(union ulocals new_ulocals)
                        (locate ,bindings
                            (frame-conflict ,conf_graph ,tail))))])))