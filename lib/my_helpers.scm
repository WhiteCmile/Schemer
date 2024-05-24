(define triv?
    (lambda (triv)
        (cond
            [(int64? triv) #t]
            [(label? triv) #t]
            [(uvar? triv) #t]
            [(register? triv) #t]
            [(frame-var? triv) #t]
            [else #f])))

; Check whether an operator is a relational operator
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

; Check whether an operator is a arithmetic operator
(define binop?
    (lambda (op)
        (match op
            [+ #t]
            [- #t]
            [* #t]
            [/ #t]
            [logand #t]
            [logor #t]
            [sra #t]
            [,x #f])))

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

; Given a triv, if it's a key in bindings, return the value, otherwise return the triv
(define substitute_with_value
    (lambda (bindings)
        (lambda (triv)
            (let ([kv_pair (assoc triv bindings)])
                (if kv_pair
                    (cadr kv_pair)
                    triv)))))

; bind_list: list of (key, value)s
; Replace every key with value in the conflict list if can
(define replace_uvars
    (lambda (conf_list bind_list)
        (map
            (lambda (var)
                (if (uvar? var)
                    ((substitute_with_value bind_list) var)
                    var))
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

; Given a list of trivs, turn all frame variables into unique variables
; return 
;   new_trivs: a list of trivs with all frame variables turned into unique variables
;   new_ulocs: a list of uvars corresponding to those frame variables
;   instructions: a list of instructions to bind those unique variables to frame variables
(define (turn_fv_to_uvars trivs)
    (define instructions '())
    (define uloc_list '())
    (define new_trivs
        (let loop ([trivs trivs])
            (match trivs
                [() '()]
                [(,triv . ,rest)
                    (if (frame-var? triv)
                        (let ([uloc (unique-name 'uloc)])
                            (set! instructions `(,@instructions (set! ,uloc ,triv)))
                            (set! uloc_list (cons uloc uloc_list))
                            (cons uloc (loop rest)))
                        (cons triv (loop rest)))])))
    (values new_trivs uloc_list instructions))

; Check whether a prim is a value-prim
(define (value-prim? prim)
    (match prim
        [car #t]
        [cdr #t]
        [cons #t]
        [make-vector #t]
        [vector-length #t]
        [vector-ref #t]
        [void #t]
        [,x (binop? x)]))

(define (pred-prim? prim)
    (match prim
        [boolean? #t]
        [eq? #t]
        [fixnum? #t]
        [null? #t]
        [pair? #t]
        [vector? #t]
        [,x (relop? x)]))

(define (effect-prim? prim)
    (match prim
        [set-car! #t]
        [set-cdr! #t]
        [vector-set! #t]
        [,x #f]))

(define (prim? prim)
    (or (value-prim? prim)
        (pred-prim? prim)
        (effect-prim? prim)))

; Check whether a word is begin, if or let
(define (keyword? word)
    (match word
        [begin #t]
        [if #t]
        [let #t]
        [,x #f]))

; Return the index of an object in a list
; Return #f if not found
(define (index-of obj lst)
    (let loop ((lst lst) (index 0))
        (cond
            ((null? lst) #f) 
            ((eq? obj (car lst)) index) 
            (else (loop (cdr lst) (+ index 1))) 
        )
    )
)