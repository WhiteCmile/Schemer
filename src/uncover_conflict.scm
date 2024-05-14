(load "lib/match.scm")

; Initilize a conflict graph with a given list of uvars
(define init_conflict_graph
    (lambda (uvars)
        (match uvars
            [() '()]
            [(,uvar . ,[rest])
                (cons (list uvar) rest)])))

; Check if an element can in a live set
(define valid_in_live_set?
    (lambda (triv what)
        (if (or (uvar? triv) (what triv))
            #t
            #f)))

; Initialize a live set with (Triv Loc*)
(define init_live_set
    (lambda (triv* what)
        (match triv*
            [() '()]
            [(,triv . ,[rest])
                (if (valid_in_live_set? triv what)
                    (union `(,triv) rest)
                    rest)])))

; Union two conflict graph
(define union_conflict_graph
    (lambda (conf_graph1 conf_graph2)
        (if (null? conf_graph1)
            '()
            (let 
                ([var (car (car conf_graph1))]
                [conf_list1 (cdr (car conf_graph1))]
                [conf_list2 (cdr (car conf_graph2))])
                (cons 
                    (cons var (union conf_list1 conf_list2))
                    (union_conflict_graph (cdr conf_graph1) (cdr conf_graph2)))))))

; Append several elements to a live set
(define append_live_set
    (lambda (live_set triv_set what)
        (match triv_set
            [() live_set]
            [(,triv . ,[new_live_set])
                (if (valid_in_live_set? triv what)
                    (union `(,triv) new_live_set)
                    new_live_set)])))

; Append var's conflict list in conflict graph with a live set
(define append_conflict_list
    (lambda (live_set conf_graph var)
        (match conf_graph
            [() '()]
            [((,var1 . ,conf_list) . ,sub_graph)
                (if (eq? var var1)
                    (cons (cons var (union conf_list live_set))
                        sub_graph)
                    (cons (car conf_graph)
                        (append_conflict_list live_set sub_graph var)))])))
                    
; Append a conflict graph with a list of items that conflicts with this var
; We need to make a mutual confliction
(define append_conflict_graph
    (lambda (live_set conf_graph var)
        (let 
            ([conf_graph 
                (if (uvar? var) 
                    (append_conflict_list live_set conf_graph var)
                    conf_graph)])
            (letrec 
                ([another_dir
                    (lambda (live_set conf_graph var)
                        (match live_set
                            [() conf_graph]
                            [(,triv . ,[new_conf_graph])
                                (if (uvar? triv)
                                    (append_conflict_list `(,var) new_conf_graph triv)
                                    new_conf_graph)]))])
                (another_dir live_set conf_graph var)))))

; Ignore assignment if the variable is not in the live set
; This is an optimization
(define ignore_assignment?
    (lambda (var live_set what)
        (cond 
            [(and (valid_in_live_set? var what) (null? (intersection `(,var) live_set))) #t]
            [else #f])))

; Maintain the live set and conflict graph in an assignment
(define handle_assignment
    (lambda (live_set conf_graph assignment var set_to_add what)
        (if (ignore_assignment? var live_set what)
            (values live_set conf_graph '(nop))
            (let ([live_set (difference live_set `(,var))])
                (values 
                    (append_live_set live_set set_to_add what)
                    (append_conflict_graph live_set conf_graph var)
                    assignment)))))

; Unlike the previous version, we take an optimization here
; For those assignment that LHS is not in the live set, we can discard the assignment
; So it may change the structure
(define uncover_conflict
    ; Modified in a5
    ; what is a function that checks if a non-variable can be in a live set
    ; For instance, if what = register?, then this uncover-conflict is used to uncover register conflicts.
    (lambda (what uvar* tail)
        ; Modified in a7, we need to maintain a call live set
        (define call_live_set '())
        (define venture_tail
            (lambda (live_set conf_graph tail)
                (match tail
                    [(begin ,effect* ... ,sub_tail)
                        (let-values
                            ([(live_set sub_graph new_tail) (venture_tail live_set conf_graph sub_tail)])
                            (let-values
                                ([(live_set sub_graph effects) ((venture_effects live_set sub_graph) effect*)])
                                (values
                                    live_set
                                    sub_graph
                                    (make-begin (append effects `(,new_tail))))))]
                    [(if ,pred ,tail1 ,tail2)
                        (let-values
                            ([(live_set1 sub_graph1 tail1) (venture_tail live_set conf_graph tail1)]
                            [(live_set2 sub_graph2 tail2) (venture_tail live_set conf_graph tail2)])
                            (let-values
                                ([(live_set sub_graph pred) (venture_pred live_set1 live_set2 sub_graph1 sub_graph2 pred)])
                                (values
                                    live_set
                                    sub_graph
                                    `(if ,pred ,tail1 ,tail2))))]
                    [(,triv ,Loc* ...)
                        (set! call_live_set (union call_live_set live_set))
                        (let 
                            ([live_set (append_live_set live_set (cons triv Loc*) what)])
                            (values 
                                live_set
                                conf_graph
                                tail))])))
        (define venture_pred
            (lambda (live_set1 live_set2 conf_graph1 conf_graph2 pred)
                (match pred
                    [(begin ,effect* ... ,sub_pred)
                        (let-values 
                            ([(live_set sub_graph sub_pred) (venture_pred live_set1 live_set2 conf_graph1 conf_graph2 sub_pred)])
                            (let-values
                                ([(live_set sub_graph effects) ((venture_effects live_set sub_graph) effect*)])
                                (values
                                    live_set
                                    sub_graph
                                    (make-begin (append effects `(,sub_pred))))))]
                    [(if ,sub_pred ,pred1 ,pred2)
                        (let-values 
                            ([(new_live_set1 new_conf_graph1 pred1) (venture_pred live_set1 live_set2 conf_graph1 conf_graph2 pred1)]
                            [(new_live_set2 new_conf_graph2 pred2) (venture_pred live_set1 live_set2 conf_graph1 conf_graph2 pred2)])
                            (let-values
                                ([(live_set sub_graph sub_pred) 
                                    (venture_pred new_live_set1 new_live_set2 new_conf_graph1 new_conf_graph2 sub_pred)])
                                (values
                                    live_set
                                    sub_graph
                                    `(if ,sub_pred ,pred1 ,pred2))))]
                    [(,relop ,triv1 ,triv2)
                        (values 
                            (append_live_set (union live_set1 live_set2) (list triv1 triv2) what)
                            (union_conflict_graph conf_graph1 conf_graph2)
                            pred)]
                    [(true) (values live_set1 conf_graph1 pred)]
                    [(false) (values live_set2 conf_graph2 pred)])))
        (define venture_effect
            (lambda (live_set conf_graph effect)
                (match effect
                    [(begin ,effect* ...)
                        (let-values 
                            ([(live_set sub_graph new_effects) ((venture_effects live_set conf_graph) effect*)])
                            (values 
                                live_set 
                                sub_graph 
                                (make-begin new_effects)))]
                    [(if ,pred ,effect1 ,effect2)
                        (let-values
                            ([(live_set1 sub_graph1 effect1) (venture_effect live_set conf_graph effect1)]
                            [(live_set2 sub_graph2 effect2) (venture_effect live_set conf_graph effect2)])
                            (let-values
                                ([(live_set sub_graph pred) 
                                    (venture_pred live_set1 live_set2 sub_graph1 sub_graph2 pred)])
                                (values
                                    live_set
                                    sub_graph
                                    `(if ,pred ,effect1 ,effect2))))]
                    [(set! ,var (,binop ,triv1 ,triv2))
                        (handle_assignment live_set conf_graph effect var `(,triv1 ,triv2) what)]
                    [(set! ,var ,triv)
                        (handle_assignment live_set conf_graph effect var `(,triv) what)]
                    [(return-point ,label ,tail)
                        (let-values
                            ([(live_set conf_graph tail) (venture_tail live_set conf_graph tail)])
                            (values 
                                live_set 
                                conf_graph 
                                `(return-point ,label ,tail)))]
                    [,x (values live_set conf_graph effect)])))
        (define venture_effects
            (lambda (live_set conf_graph)
                (lambda (effects)
                    (match effects
                        [() (values live_set conf_graph '())]
                        [(,effect . ,[(venture_effects live_set conf_graph) -> live_set conf_graph last_effects])
                            (let-values 
                                ([(live_set sub_graph effect) (venture_effect live_set conf_graph effect)])
                                (values
                                    live_set
                                    sub_graph
                                    (cons effect last_effects)))]))))
        (let-values 
            ([(live_set conf_graph tail) (venture_tail '() (init_conflict_graph uvar*) tail)])
            (values live_set call_live_set conf_graph tail))))

(define uncover-frame-conflict
    (lambda (program)
        (match program
            [(letrec ([,label* (lambda () ,[body*])] ...) ,[body])
                `(letrec ([,label* (lambda () ,body*)] ...) ,body)]
            [(locals ,uvar* 
                (new-frames ,new_frame* ,tail))
                (let-values
                    ([(live_set call_live_set conf_frame_graph tail) (uncover_conflict frame-var? uvar* tail)])
                    `(locals ,(difference uvar* (filter uvar? call_live_set))
                        (new-frames ,new_frame*
                            (spills ,(filter uvar? call_live_set)
                                (frame-conflict ,(sort (lambda (a b) (< (length a) (length b))) conf_frame_graph)
                                    (call-live ,call_live_set ,tail))))))])))

(define uncover-register-conflict
    (lambda (program)
        (match program
            [(letrec ([,label* (lambda () ,[body*])] ...) ,[body])
                `(letrec ([,label* (lambda () ,body*)] ...) ,body)]
            [(locate ,completed* ...) `(locate ,completed* ...)]
            [(locals ,uvar*
                (ulocals ,uloc*
                    (locate ,bind*
                        (frame-conflict ,conf_frame_graph ,tail))))
                (let-values
                    ([(xxx live_set conf_reg_graph tail) (uncover_conflict register? (union uvar* uloc*) tail)])
                    `(locals ,uvar*
                        (ulocals ,uloc*
                            (locate ,bind*
                                (frame-conflict ,conf_frame_graph
                                    (register-conflict ,conf_reg_graph ,tail))))))])))