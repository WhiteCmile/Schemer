(load "lib/match.scm")
(load "lib/helpers.scm")
(load "lib/my_helpers.scm")

; Remove a variable from the graph
; More specifically, remove it from the list, and remove it from the conflict lists of other variables
(define remove_var_in_graph
    (lambda (var graph)
        (match graph
            [() '()]
            [((,var2 . ,conf_list) . ,sub_graph)
                (if (eq? var var2)
                    (remove_var_in_graph var sub_graph)
                    (cons (cons var2 (difference conf_list `(,var))) (remove_var_in_graph var sub_graph)))])))

; Find a register for a variable
; Only consider conflictions with binded_vars
; Return a empty binding if no register is available
(define find_avail_reg
    (lambda (var conf_list binded_vars bind_list)
        (let*
            ([conf_list (intersection binded_vars (replace_uvars conf_list bind_list))]
            [reg_list (difference registers conf_list)])
            (if (null? reg_list)
                '()
                (list var (car reg_list))))))

; Allocate registers to variables
; If can't find a register for a variable, then throw an error
(define register_allocator
    (lambda (conf_graph)
        (define bind_var
            (lambda (sorted_graph)
                (match sorted_graph
                    [() '()]
                    [((,var . ,conf_list) . ,sub_graph)
                        (unless (set? conf_list)
                            (format-error who "conf_list ~a is not a set" conf_list))
                        (let-values
                            ([(binded_vars bind_list)
                                (bind_var (remove_var_in_graph var sub_graph))])
                            (let ([bind (find_avail_reg var conf_list binded_vars bind_list)])
                                (if (null? bind)
                                    (values binded_vars bind_list)
                                    (values (cons var binded_vars) (cons bind bind_list)))))])))
        (let*
            ([sorted_graph (sort (lambda (a b) (< (length a) (length b))) conf_graph)])
            (bind_var sorted_graph))))

(define assign-registers
    (lambda (program)
        (match program
            [(letrec ([,label* (lambda () ,[body*])] ...) ,[body])
                `(letrec ([,label* (lambda () ,body*)] ...) ,body)]
            [(locals ,uvar*
                (ulocals ,uloc*
                    (locate ,bind*
                        (frame-conflict ,conf_frame_graph
                            (register-conflict ,conf_reg_graph ,tail)))))
                (let-values
                    ([(binded_vars bind_list) (register_allocator conf_reg_graph)])
                    (unless (null? (difference uloc* binded_vars))
                        (format-error who "Unspillable variables ~a are not binded to registers" (difference uloc* binded_vars)))
                    (if (null? (difference uvar* binded_vars))
                        `(locate ,(append bind* bind_list) ,tail)
                        (let*
                            ; spill_vars is a list of spilled variables in uvar*
                            ; others are the rest of uvar* that are binded to registers
                            ([spill_vars (difference uvar* binded_vars)]
                            [others (difference uvar* spill_vars)])
                            `(locals ,others
                                (ulocals ,uloc*
                                    (spills ,spill_vars
                                        (locate ,bind*
                                            (frame-conflict ,conf_frame_graph ,tail))))))))])))
