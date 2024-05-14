(load "lib/match.scm")

; Insert a statement (not being a Pred) to the beginning of a tail
; If the tail is shaped like (begin ...) then new tail is (begin stat ...)
; Otherwise, new tail is (begin stat tail)
(define ins_stat_to_tail
    (lambda (stat tail)
        (match tail
            [(begin ,stat* ...)
                `(begin ,stat ,stat* ...)]
            [,x `(begin ,stat ,x)])))

; Insert statements (without Preds) to the beginning of a tail
; The statements forms like (stat1 stat2 ...)
; If the tail is shaped like (begin sth) then new tail is (begin stat1 stat2 ... sth)
; Otherwise, new tail is (begin stat1 stat2 ... tail)
(define ins_stats_to_tail
    (lambda (stats tail)
        (match stats
            [(,stat* ...)
                (match tail
                    [(begin ,sth) `(begin ,stat* ... ,sth)]
                    [,x `(begin ,stat* ... ,x)])])))

; Insert a Pred to the beginning of two tails to form a new If structure
; Pred tail1 tail -> (if Pred tail1 tail2)
(define ins_pred_to_tail
    (lambda (pred tail1 tail2)
        `(if ,pred ,tail1 ,tail2)))

(define expose-basic-blocks
    (lambda (program)
        ; This function is used to generate two labels for two branch tails to avoid exponential explosion
        (define abbreviate_two_tails
            (lambda (tail1 tail2)
                (let-values 
                    ([(label_list1 tail_list1 new_tail1) (Tail tail1)]
                    [(label_list2 tail_list2 new_tail2) (Tail tail2)])
                    (let* 
                        ([label1 (unique-label 'ztl)]
                        [label2 (unique-label 'ztl)]
                        [label_list (append (cons label1 label_list1) (cons label2 label_list2))]
                        [tail_list (append (cons new_tail1 tail_list1) (cons new_tail2 tail_list2))])
                        (values label_list tail_list label1 label2 tail1 tail2)))))
        (define Tail
            (lambda (tail)
                (match tail
                    [(begin ,effect* ... ,tail) (Effect effect* tail)]
                    [(if ,pred ,tail1 ,tail2) (If pred tail1 tail2)]
                    [,x (values '() '() x)])))
        (define If
            (lambda (pred tail1 tail2)
                (match pred
                    [(begin ,effect* ... ,sub_pred)
                        (let-values 
                            ([(label_list tail_list sub_tail) (If sub_pred tail1 tail2)])
                            (let-values 
                                ([(new_label_list new_tail_list if_tail) (Effect effect* sub_tail)])
                                    (values 
                                        (append new_label_list label_list)
                                        (append new_tail_list tail_list)
                                        if_tail)))]
                    [(if ,sub_pred ,sub_pred1 ,sub_pred2)
                        (let-values 
                            ([(label_list tail_list label1 label2 tail1 tail2) 
                                (abbreviate_two_tails tail1 tail2)])
                            (let* 
                                ([new_tail1 (ins_pred_to_tail sub_pred1 `(,label1) `(,label2))]
                                [new_tail2 (ins_pred_to_tail sub_pred2 `(,label1) `(,label2))])
                                (let-values
                                    ([(new_label_list new_tail_list new_tail) 
                                        (If sub_pred new_tail1 new_tail2)])
                                    (values
                                        (append new_label_list label_list)
                                        (append new_tail_list tail_list)
                                        new_tail))))]
                    [(,relop ,triv1 ,triv2)
                        (let-values ([(label_list1 tail_list1 new_tail1) (Tail tail1)]
                                    [(label_list2 tail_list2 new_tail2) (Tail tail2)])
                            (let* ([label_list (append label_list1 label_list2)]
                                    [tail_list (append tail_list1 tail_list2)]
                                    [label1 (unique-label 'ztl)]
                                    [label2 (unique-label 'ztl)]
                                    [label_list (cons label1 (cons label2 label_list))]
                                    [tail_list (cons new_tail1 (cons new_tail2 tail_list))])
                                (values label_list tail_list `(if ,pred (,label1) (,label2)))))]
                    [(true) (Tail tail1)]
                    [(false) (Tail tail2)])))
        (define Effect
            (lambda (effects tail)
                (if (null? effects)
                    (Tail tail)
                    (match (car effects)
                        [(begin ,effect* ...)
                            (Effect (append effect* (cdr effects)) tail)]
                        ; A new method to handle if statement for avoiding exponential explosion
                        ; It will turn (cdr effects) tail to a new label at first
                        ; Then the two branch will get back to the same control flow to avoid exponential explosion
                        [(if ,pred ,effect1 ,effect2)
                            (let* ([label (unique-label 'ztl)]
                                    [tail1 (ins_stat_to_tail effect1 `(,label))]
                                    [tail2 (ins_stat_to_tail effect2 `(,label))]) 
                                (let-values 
                                    ([(label_list tail_list new_tail) 
                                        (Tail (ins_stats_to_tail (cdr effects) tail))]
                                    [(stat_label_list stat_tail_list stat_new_tail) 
                                        (If pred tail1 tail2)])
                                    (values (append stat_label_list (cons label label_list)) 
                                            (append stat_tail_list (cons new_tail tail_list)) 
                                            stat_new_tail)))]
                        [(return-point ,label ,sub_tail)
                            (let-values
                                ([(label_list tail_list new_tail) (Effect (cdr effects) tail)]
                                [(sub_label_list sub_tail_list sub_tail) (Tail sub_tail)])
                                (values (cons label (append sub_label_list label_list))
                                        (cons new_tail (append sub_tail_list tail_list))
                                        (make-begin `(,sub_tail))))]
                        [(nop) (Effect (cdr effects) tail)]
                        [,effect 
                            (let-values ([(label_list tail_list tail) (Effect (cdr effects) tail)])
                                (values label_list tail_list (ins_stat_to_tail effect tail)))]))))
        (match program
            [(letrec ([,label* (lambda () ,[Tail -> label_list* tail_list* tail*])] ...)
                ,[Tail -> body_label_list body_tail_list body_tail])
                `(letrec ([,label* (lambda () ,tail*)] ... 
                    [,label_list* (lambda () ,tail_list*)] ... ... 
                    [,body_label_list (lambda () ,body_tail_list)] ...)
                    ,body_tail)])))