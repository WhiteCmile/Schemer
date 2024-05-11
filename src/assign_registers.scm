(load "lib/match.scm")
(load "lib/helpers.scm")

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
(define find_avail_reg
    (lambda (var conf_list bind_list)
        (let*
            ([conf_list (replace_with_reg conf_list bind_list)]
            [reg_list (difference registers conf_list)])
            (if (null? reg_list)
                (format-error who "No register available for variable ~a" var)
                (list var (car reg_list))))))

; Replace every uvar with register in the conflict list
(define replace_with_reg
    (lambda (conf_list bind_list)
        (map
            (lambda (var)
                (if (uvar? var)
                    (let 
                        ([var_reg_pair (assoc var bind_list)])
                        (if var_reg_pair
                            (cadr var_reg_pair)
                            (format-error who "can't find register for variable ~a in bind_list ~a" var bind_list)))
                    var))
            conf_list)))

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
                        (let 
                            ([bind_list 
                                (bind_var (remove_var_in_graph var sub_graph))])
                            (cons (find_avail_reg var conf_list bind_list) bind_list))])))
        (let*
            ([sorted_graph (sort (lambda (a b) (< (length a) (length b))) conf_graph)])
            (bind_var sorted_graph))))

(define assign-registers
    (lambda (program)
        (define Body
            (lambda (body)
                (match body
                    [(locals ,uvar* 
                        (register-conflict ,conf_graph ,tail))
                        `(locate ,(register_allocator conf_graph) ,tail)])))
        (match program
            [(letrec ([,label* (lambda () ,[Body -> body*])] ...) ,[Body -> body])
                `(letrec ([,label* (lambda () ,body*)] ...) ,body)])))