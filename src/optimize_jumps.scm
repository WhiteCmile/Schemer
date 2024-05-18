(define (optimize-jumps program)
    (define (build_assoc_list label* tail*)
        (let loop ([label* label*] [tail* tail*])
            (match label*
                [() (values '() '() '())]
                [(,label . ,rest)
                    (let-values 
                        ([(label* tail* assoc_list) (loop rest (cdr tail*))])
                        (match (car tail*)
                            [(,jump_label) (guard (label? jump_label))
                                (values label* tail* (cons (cons label jump_label) assoc_list))]
                            [,x (values (cons label label*) (cons x tail*) assoc_list)]))])))
    (define (optimize_tail assoc_list)
        (lambda (tail)
            (match tail
                [(,[item*] ...) `(,@item*)]
                [,label (guard (label? label))
                    (let ([jump_label (assoc label assoc_list)])
                        (if (null? jump_label)
                            label
                            (cadr jump_label)))]
                [,x x])))
    (match program
        [(letrec ([,label* (lambda () ,tail*)] ...)
            ,tail)
            (let-value
                ([(label* tail* assoc_list) (build_assoc_list label* tail*)])
                (let 
                    ([optimized_tail* (map (optimize_tail assoc_list) tail*)]
                    [optimized_tail ((optimize_tail assoc_list) tail)])
                    `(letrec ([,label* (lambda () ,optimized_tail*)] ...)
                        ,optimized_tail)))]))
