(load "lib/match.scm")

(define flatten-program
    (lambda (program)
        (define Label
            (lambda (labels tails)
                (match labels
                    [(,label . ,rest_labels)
                        (let ([rest_program (Label rest_labels (cdr tails))]
                                [instructions (cons label (Tail (car tails) rest_labels (cdr tails)))])
                            (append instr rest_program))]
                    [() '()])))
        ; This function takes a tail, a list of labels, and a list of tails as arguments
        ; Why it takes a list of labels? We need them to know how to generate the jump instructions for better performance
        (define Tail
            (lambda (tail labels tails)
                (match tail
                    [(begin ,effect* ... ,sub_tail)
                        (append effect* (Tail sub_tail labels tails))]
                    [(if ,operation (,label_true) (,label_false))
                        (match labels
                            [(,label . ,rest_labels)
                                (let ([rest_program (Label labels tails)]
                                        [instr 
                                            (cond
                                                [(eq? label_true label) `((if (not ,operation) (jump ,label_false)))]
                                                [(eq? label_false label) `((if ,operation (jump ,label_true)))]
                                                [else `((if ,operation (jump ,label_true)) (jump ,label_false))])])
                                    (append instr rest_program))]
                            [() `((if ,operation (jump ,label_true)) (jump ,label_false))])]
                    [(,triv) (guard (label? triv))
                        (match labels
                            [(,label . ,rest_labels)
                                (if (eq? label triv)
                                    '()
                                    `((jump ,triv)))]
                            [() `(jump ,triv)])]
                    [(,triv) `((jump ,triv))])))
        (match program
            [(letrec ([,label* (lambda () ,tail*)] ...) ,body_tail)
                (cons 'code (Tail body_tail label* tail*))])))