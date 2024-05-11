(load "lib/helpers.scm")

(define triv?
    (lambda (triv)
        (cond
            [(int64? triv) #t]
            [(label? triv) #t]
            [(uvar? triv) #t]
            [(register? triv) #t]
            [(frame-var? triv) #t]
            [else #f])))

; Check an operator is a relational operator
(define relop?
    (lambda (op)
        (cond
            [(eq? op '<) #t]
            [(eq? op '<=) #t]
            [(eq? op '>) #t]
            [(eq? op '>=) #t]
            [(eq? op '=) #t]
            [(eq? op '!=) #t]
            [else #f])))

; Return the inverse of a relational operator
(define inverse_op
    (lambda (op)
        (cond
            [(eq? op '<) '>]
            [(eq? op '>) '<]
            [(eq? op '<=) '>=]
            [(eq? op '>=) '<=]
            [(eq? op '=) '=]
            [(eq? op '!=) '!=])))

; Return whether a arithmetic operator satisfies the commutative property
(define can_swap?
    (lambda (op)
        (or (eq? op '+) 
            (eq? op '*)
            (eq? op 'logor)
            (eq? op 'logand))))

; Return whether an operand can be a register
(define can_be_reg?
    (lambda (operand)
        (or (register? operand)
            (uvar? operand))))

; Return whether an item is in the set
(define item_in_set?
    (lambda (item set)
        (cond
            [(null? set) #f]
            [(eq? item (car set)) #t]
            [else (item_in_set? item (cdr set))])))

; bind_list: list of (key, value)s
; Replace every key with value in the conflict list if can
(define replace_uvars
    (lambda (conf_list bind_list)
        (map
            (lambda (var)
                (if (uvar? var)
                    (let 
                        ([key_value (assoc var bind_list)])
                        (if key_value
                            (cadr key_value)
                            var)
                    var)))
            conf_list)))

; find the smallest non-negtive number in an ordered list
(define find_smallest_no_neg
    (lambda (lst)
        (let loop ([i 0] [lst lst])
            (cond
                [(null? lst) i]
                [(= (car lst) i) (loop (+ i 1) (cdr lst))]
                [(= (car lst) (- i 1)) (loop i (cdr lst))]
                [else i]))))
