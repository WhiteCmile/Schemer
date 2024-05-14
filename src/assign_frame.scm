(load "lib/match.scm")
(load "lib/my_helpers.scm")

; break the conf_graph into two pieces, one for spilled vars and the other for non-spilled vars
(define break_graph
    (lambda (spill_vars)
        (lambda (conf_graph)
            (match conf_graph
                [() (values '() '())]
                [(,conf_list . ,[graph_spilled conf_graph])
                    (if (item_in_set? (car conf_list) spill_vars)
                        (values (cons conf_list graph_spilled) conf_graph)
                        (values graph_spilled (cons conf_list conf_graph)))]))))

; Got a frame-var that does not conflict with any other frame-var in conf_list
(define allocate_frame 
    (lambda (bindings conf_list)
        (let* ([conf_list (replace_uvars conf_list bindings)]
                [filtered_list (filter frame-var? conf_list)]
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

(define pre-assign-frame
    (lambda (program)
        (match program
            [(letrec ([,label* (lambda () ,[body*])] ...) ,[body])
                `(letrec ([,label* (lambda () ,body*)] ...) ,body)]
            [(locals ,uvar*
                (new-frames ,new_frame*
                    (spills ,spill_var*
                        (frame-conflict ,conf_graph 
                            (call-live ,call_live_var* ,tail)))))
                ; new_bindings are bindings from uvars to fvars
                ; The confliction list of the binded variables will be removed from conf_graph
                (let-values 
                    ([(new_bindings conf_graph) (fvar_allocator spill_var* binding* conf_graph)])
                    `(locals ,uvar*
                        (new-frames ,new_frame*
                            (locate ,new_bindings
                                (frame-conflict ,conf_graph 
                                    (call-live ,call_live_var* ,tail))))))])))

; Get frame size of a function
; The size of the frame is, simply, one more than the maximum index of the frame locations of the call-live
; variables or frame variables.
(define (get_frame_size bindings call_live_vars)
    (let* 
        ([fv_list1 (filter frame-var? call_live_vars)]
        [fv_list2 (map cadr bindings)]
        [index_list (map (lambda (fv) (frame-var->index fv)) (append fv_list1 fv_list2))])
    (add1 (apply max index_list))))

; Assign frame locations for new frame variables
(define (assign_new_frame new_frame_lists frame_size)
    (define bindings '())
    (map 
        (lambda (new_frame_list)
            (let loop ([new_frame_list new_frame_list] [pos frame_size])
                (match new_frame_list
                    [() (void)]
                    [(,fv . ,rest)
                        (set! bindings (cons (list fv (index->frame-var pos)) bindings))
                        (loop rest (add1 pos))])))
        new_frame_lists)
    bindings)

; Increament and decrement frame pointer register in return-point statement
(define (handle_stack frame_size statement)
    (define fp frame-pointer-register)
    (define nb (ash frame_size align-shift))
    (match statement
        [(begin ,[statements*] ...) `(begin ,statements* ...)]
        [(if ,[statements*] ...) `(if ,statements* ...)]
        [(return-point ,label ,[tail])
            `(begin
                (set! ,fp (+ ,fp ,nb))
                (return-pointer ,label ,tail)
                (set! ,fp (- ,fp ,nb)))]
        [,x x]))
            
(define (assign-new-frame program)
    (match program
        [(letrec ([,label* (lambda () ,[body*])] ...) ,[body])
            `(letrec ([,label* (lambda () ,body*)] ...) ,body)]
        [(locals ,uvar*
            (new-frames ,new_frame_list*
                (locate ,binding*
                    (frame-conflict ,conf_graph
                        (call-live ,call_live_var* ,tail)))))
            (let*
                ([frame_size (get_frame_size binding* call_live_var*)]
                [new_bindings (assign_new_frame new_frame_list* frame_size)])
                `(locals ,(difference uvar* (apply union new_frame_list*))
                    (ulocals ()
                        (locate ,(append new_bindings binding*)
                            (frame-conflict ,conf_graph
                                ,(handle_stack frame_size tail))))))]))