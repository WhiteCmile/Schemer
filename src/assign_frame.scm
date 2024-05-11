(load "lib/match.scm")
(load "lib/my_helpers.scm")

; break the conf_graph into two pieces, one for spilled vars and the other for non-spilled vars
(define break_graph
    (lambda (spill_vars)
        (lambda (conf_graph)
            (match conf_graph
                ['() (values '() '())]
                [(,conf_list . ,[graph_spilled conf_graph])
                    (if (item_in_set? (car conf_list) spill_vars)
                        (values (cons conf_list graph_spilled) conf_graph)
                        (values graph_spilled (cons conf_list conf_graph)))]))))

(define allocate_frame 
    (lambda (bindings conf_list)
        (let* ([conf_list (replace_uvars conf_list bindings)]
                [filtered_list (filter frame_var? conf_list)]
                [indexes (sort (lambda (a b) (< a b)) (map (lambda (fv) (frame-var->index fv)) filtered_list))])
            (index->frame-var (find_smallest_no_neg indexes)))))

(define fvar_allocator
    (lambda (spill_vars bindings conf_graph)
        (let-values 
            ; graph_spilled: subgraph of conf_graph with confliction lists of spill_vars
            ; conf_graph: graph of other confliction lists
            ([(graph_spilled conf_graph) ((break_graph spill_vars) conf_graph)])
            (let loop 
                ([bindings bindings]
                [graph (sort (lambda (a b) (< (length a) (length b))) graph_spilled)])
                (if (null? graph)
                    (values bindings conf_graph)
                    (let ([fvar (allocate_frame bindings (car graph))])
                        (loop 
                            (cons (list (caar graph) fvar) 
                                    bindings)
                            (cdr graph))))))))

(define assign-frame
    (lambda (program)
        (match program
            [(letrec ([,label* (lambda () ,[body*])] ...) ,[body])
                `(letrec ([,label* (lambda () ,body*)] ...) ,body)]
            [(locals ,uvar*
                (ulocals ,uloc*
                    (spills ,spill_var*
                        (locate ,binding*
                            (frame-conflict ,conf_graph ,tail)))))
                ; bind* and new_bindings are bindings from uvars to fvars
                ; The confliction list of the binded variables will be removed from conf_graph
                (let-values 
                    ([(new_bindings conf_graph) (fvar_allocator spill_var* binding* conf_graph)])
                    `(locals ,uvar*
                        (ulocals ,uloc*
                            (locate ,new_bindings
                                (frame-conflict ,conf_graph ,tail)))))]
            [,x x])))
