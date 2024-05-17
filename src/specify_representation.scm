(define offset-car (- disp-car tag-pair))
(define offset-cdr (- disp-cdr tag-pair))

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

; Check whether a prim is a pred-prim
(define (pred-prim? prim)
    (match prim
        [boolean? #t]
        [eq? #t]
        [fixnum? #t]
        [null? #t]
        [pair? #t]
        [vector? #t]
        [,x (relop? x)]))

; Check whether a prim is an effect-prim
(define (effect-prim? prim)
    (match prim
        [set-car! #t]
        [set-cdr! #t]
        [vector-set! #t]))

; Specify the representation of an immediate
(define (specify_immediate statement)
    (match statement
        [(quote #f) $false]
        [(quote #t) $true]
        [(quote ()) $nil]
        [(quote ,x) (guard (fixnum? x)) (ash x shift-fixnum)]))


; Specify the representation of a value prim
; prim is the prim, and values is a list of specified values
(define (specify_value_prim prim values)
    ; Specify the representation when pred is a binary operator
    (define (specify_binop op values)
        (match op
            [+ `(+ ,@values)]
            [- `(- ,@values)]
            [* `(* ,(car values) (sra ,(cadr values) ,shift-fixnum))]
            [,x (error "Invalid binop ~s\n" x)]))
    ; Specify representation of a cons expression
    (define (specify_cons e1 e2)
        (let ([tmp-car (unique-name 'tmp)]
            [tmp-cdr (unique-name 'tmp)]
            [tmp (unique-name 'tmp)])
            `(let ([,tmp-car ,e1] [,tmp-cdr ,e2])
                (let ([,tmp (+ (alloc ,size-pair) ,tag-pair)])
                    (begin
                        (mset! ,tmp ,offset-car ,tmp-car)
                        (mset! ,tmp ,offset-cdr ,tmp-cdr)
                        ,tmp)))))
    (match prim
        [void `(,$void)]
        [,op (guard (binop? op)) (specify_binop op values)]
        [car `(mref ,(car values) ,offset-car)]
        [cdr `(mref ,(car values) ,offset-cdr)]
        [cons (specify_cons (car values) (cadr values))]))

(define (specify_effect_prim prim values)
    (match prim
        [set-car! `(mset! ,(car values) ,offset-car ,(cadr values))]
        [set-cdr! `(mset! ,(car values) ,offset-cdr ,(cadr values))]))

(define (specify-representation program)
    ; The Stmt helper handles Value, Pred and Effect structures
    (define (Stmt statement)
        (match statement
            [(begin ,[Stmt -> statement*] ...) `(begin ,statement* ...)]
            [(if ,[Stmt -> statement*] ...) `(if ,statement* ...)]
            [(let ([,uvar* ,[Stmt -> statement*]] ...) ,[Stmt -> statement])
                `(let ([,uvar* ,statement*] ...) ,statement)]
            [(quote ,immediate) (specify_immediate statement)]
            [(,prim ,[Stmt -> value*] ...) (guard (value-prim? prim)) 
                (specify_value_prim prim value*)]
            [(,prim ,[Stmt -> value*] ...) (guard (pred-prim? prim))
                (specify_pred_prim prim value*)]
            [(,prim ,[Stmt -> value*] ...) (guard (effect-prim? prim))
                (specify_effect_prim prim value*)]
            [(,[Stmt -> value*] ...) `(,value* ...)]
            [,x x]))
    (match program
        [(letrec ([,label* (lambda ,uvar** ,[Stmt -> value*])] ...) ,[Stmt -> value])
            `(letrec ([,label* (lambda ,uvar** ,value*)] ...) ,value)]))