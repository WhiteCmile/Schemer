(define offset-car (- disp-car tag-pair))
(define offset-cdr (- disp-cdr tag-pair))
(define offset-vec-len (- disp-vector-length tag-vector))
(define offset-vec-data (- disp-vector-data tag-vector))

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

(define (specify_immediate statement)
    (match statement
        [(quote #f) $false]
        [(quote #t) $true]
        [(quote ()) $nil]
        [(quote ,x) (guard (fixnum? x)) (ash x shift-fixnum)]))

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
    ; Specify representation of a vector creation
    (define (specify_vec_create len)
        (let ([tmp-len (unique-name 'tmp)]
            [tmp (unique-name 'tmp)])
            `(let ([,tmp-len ,len])
                (let 
                    ([,tmp 
                        (+ (alloc (+ ,disp-vector-data ,tmp-len)) 
                            ,tag-vector)])
                    (begin
                        (mset! ,tmp ,offset-vec-len ,tmp-len)
                        ,tmp)))))
    (match prim
        [void $void]
        [,op (guard (binop? op)) (specify_binop op values)]
        [car `(mref ,(car values) ,offset-car)]
        [cdr `(mref ,(car values) ,offset-cdr)]
        [cons (specify_cons (car values) (cadr values))]
        [vector-length `(mref ,(car values) ,offset-vec-len)]
        [vector-ref `(mref ,(car values) (+ ,offset-vec-data ,(cadr values)))]
        [make-vector (specify_vec_create (car values))]))

(define (specify_effect_prim prim values)
    ; Specify the representation of a vector-set!
    (define (specify_vec_set vec idx val)
        `(mset! ,vec (+ ,offset-vec-data ,idx) ,val))
    (match prim
        [set-car! `(mset! ,(car values) ,offset-car ,(cadr values))]
        [set-cdr! `(mset! ,(car values) ,offset-cdr ,(cadr values))]
        [vector-set! 
            (specify_vec_set (car values) (cadr values) (caddr values))]))

(define (specify_pred_prim prim values)
    (let ([expr (car values)])
        (match prim
            [boolean? `(= (logand ,expr ,mask-boolean) ,tag-boolean)]
            [fixnum? `(= (logand ,expr ,mask-fixnum) ,tag-fixnum)]
            [pair? `(= (logand ,expr ,mask-pair) ,tag-pair)]
            [vector? `(= (logand ,expr ,mask-vector) ,tag-vector)]
            [null? (specify_pred_prim 'eq? (cons $nil values))]
            [eq? `(= ,expr ,(cadr values))]
            [,x `(,prim ,@values)])))

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