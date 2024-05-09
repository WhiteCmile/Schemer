(load "lib/match.scm")
(load "lib/helpers.scm")

; Initilize a conflict graph with a given list of uvars
(define init_conflict_graph
    (lambda (uvars)
        (match uvars
            [() '()]
            [(,uvar . ,[rest])
                (cons (list uvar) rest)])))

; Check if an element can in a live set
(define valid_in_live_set?
    (lambda (triv)
        (if (or (uvar? triv) (register? triv))
            #t
            #f)))

; Initialize a live set with (Triv Loc*)
(define init_live_set
    (lambda (triv*)
        (match triv*
            [() '()]
            [(,triv . ,[rest])
                (if (valid_in_live_set? triv)
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
                (cons (cons var (union conf_list1 conf_list2))
                    (union_conflict_graph (cdr conf_graph1) (cdr conf_graph2)))))))

; Append several elements to a live set
(define append_live_set
    (lambda (live_set triv_set)
        (match triv_set
            [() live_set]
            [(,triv . ,[new_live_set])
                (if (valid_in_live_set? triv)
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
; This is an optimization that has not being implemented yet
(define ignore_assignment?
    (lambda (var live_set)
        #f))

; Maintain the live set and conflict graph in an assignment
(define handle_assignment
    (case-lambda
        [(live_set conf_graph var triv1 triv2)
            (if (ignore_assignment? var live_set)
                (values live_set conf_graph)
                (let ([live_set (difference live_set `(,var))])
                    (values (append_live_set live_set `(,var ,triv2)) (append_conflict_graph live_set conf_graph var))))]
        [(live_set conf_graph var triv)
            (if (ignore_assignment? var live_set)
                (values live_set conf_graph)
                (let ([live_set (difference live_set `(,var))])
                    (values (append_live_set live_set `(,triv)) (append_conflict_graph live_set conf_graph var))))]))

(define uncover-register-conflict
    (lambda (program)
        (define Body
            (lambda (body)
                (match body
                    [(locals ,uvar* ,tail)
                        (let-values 
                            ([(live_set conf_graph) (venture_tail (init_conflict_graph uvar*) tail)])
                            `(locals ,uvar* (register-rconflict ,conf_graph ,tail)))])))
        (define venture_tail
            (lambda (conf_graph tail)
                (match tail
                    [(begin ,effect* ... ,sub_tail)
                        (let-values
                            ([(live_set sub_graph) (venture_tail conf_graph sub_tail)])
                            ((venture_effects live_set sub_graph) effect*))]
                    [(if ,pred ,tail1 ,tail2)
                        (let-values
                            ([(live_set1 sub_graph1) (venture_tail conf_graph tail1)]
                            [(live_set2 sub_graph2) (venture_tail conf_graph tail2)])
                            (venture_pred live_set1 live_set2 sub_graph1 sub_graph2 pred))]
                    [(,triv ,Loc* ...)
                        (values (init_live_set (cons triv Loc*)) conf_graph)])))
        (define venture_pred
            (lambda (live_set1 live_set2 conf_graph1 conf_graph2 pred)
                (match pred
                    [(begin ,effect* ... ,sub_pred)
                        (let-values 
                            ([(live_set sub_graph) (venture_pred live_set1 live_set2 conf_graph1 conf_graph2 sub_pred)])
                            ((venture_effects live_set sub_graph) effect*))]
                    [(if ,sub_pred ,pred1 ,pred2)
                        (let-values 
                            ([(new_live_set1 new_conf_graph1) (venture_pred live_set1 live_set2 conf_graph1 conf_graph2 pred1)]
                            [(new_live_set2 new_conf_graph2) (venture_pred live_set1 live_set2 conf_graph1 conf_graph2 pred2)])
                            (venture_pred new_live_set1 new_live_set2 new_conf_graph1 new_conf_graph2 sub_pred))]
                    [(,relop ,triv1 ,triv2)
                        (values 
                            (append_live_set (union live_set1 live_set2) (list triv1 triv2))
                            (union_conflict_graph conf_graph1 conf_graph2))]
                    [(true) (values live_set1 conf_graph1)]
                    [(false) (values live_set2 conf_graph2)])))
        (define venture_effects
            (lambda (live_set conf_graph)
                (lambda (effects)
                    (match effects
                        [() (values live_set conf_graph)]
                        [(,effect . ,[(venture_effects live_set conf_graph) -> live_set conf_graph])
                            (match effect
                                [(begin ,effect* ...)
                                    ((venture_effects live_set conf_graph) effect*)]
                                [(if ,pred ,effect1 ,effect2)
                                    (let-values
                                        ([(live_set1 sub_graph1) ((venture_effects live_set conf_graph) `(,effect1))]
                                        [(live_set2 sub_graph2) ((venture_effects live_set conf_graph) `(,effect2))])
                                        (venture_pred live_set1 live_set2 sub_graph1 sub_graph2 pred))]
                                [(set! ,var (,binop ,triv1 ,triv2))
                                    (handle_assignment live_set conf_graph var triv1 triv2)]
                                [(set! ,var ,triv)
                                    (handle_assignment live_set conf_graph var triv)]
                                [,x (values live_set conf_graph)])]))))
        (match program
            [(letrec ([,label* (lambda () ,[Body -> let_body*])] ...) ,[Body -> body])
                `(letrec ([,label* (lambda () ,let_body*)] ...) ,body)])))