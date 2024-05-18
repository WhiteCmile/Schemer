(define offset-car (- disp-car tag-pair))
(define offset-cdr (- disp-cdr tag-pair))
(define offset-vec-len (- disp-vector-length tag-vector))
(define offset-vec-data (- disp-vector-data tag-vector))


(define (specify_immediate statement)
    (match statement
        [(quote #f) $false]
        [(quote #t) $true]
        [(quote ()) $nil]
        [(quote ,x) (guard (fixnum? x)) (ash x shift-fixnum)]))

; Check whether an operand is the representation of a fixnum
(define (is_fixnum? operand)
    (if (fixnum? operand)
        (= (logand operand mask-fixnum) tag-fixnum)
        #f))

; This function is used to fold constant expressions
; It's just a simple optimization
; Not handling all cases
; Returned a expr
(define (constant_folding op expr1 expr2)
    ; folding is a helper that returns a header and a value
    (define (folding op expr1 expr2)
        (define (split_header expr)
            (match expr
                [(begin ,effect* ... ,[split_header -> header value])
                    (values `(,@effect* ,@header) value)]
                [,x (values '() x)]))
        (let-values
            ([(header1 val1) (split_header expr1)]
            [(header2 val2) (split_header expr2)])
            (cond 
                [(and (binop? op) (is_fixnum? val1) (is_fixnum? val2))
                    (values 
                        `(,@header1 ,@header2)
                        (match op
                            [+ (+ val1 val2)]
                            [- (- val1 val2)]
                            [* (* val1 (ash val2 (- shift-fixnum)))]
                            [sra (ash val1 (- val2))]
                            [logand (logand val1 val2)]
                            [logor (logior val1 val2)]
                            [,x (error "Invalid binop ~s\n" x)]))]
                [(eq? op '*)
                    (cond
                        [(is_fixnum? val1) 
                            (values `(,@header1 ,@header2) 
                                `(* ,(ash val1 (- shift-fixnum)) ,val2))]
                        [(is_fixnum? val2) 
                            (values '()
                                `(* ,expr1 ,(make-begin `(,@header2 ,(ash val2 (- shift-fixnum))))))]
                        [else (values '() `(* ,expr1 (sra ,expr2 ,shift-fixnum)))])]
                [else (values '() `(,op ,expr1 ,expr2))])))
    (let-values 
        ([(header val) (folding op expr1 expr2)])
        (make-begin `(,@header ,val))))

; prim is the prim, and values is a list of specified values
(define (specify_value_prim prim values)
    ; Specify the representation when pred is a binary operator
    (define (specify_binop op values)
        (constant_folding op (car values) (cadr values)))
    ; Specify representation of a cons expression
    (define (specify_cons e1 e2)
        (let 
            ([tmp-car (unique-name 'tmp)]
            [tmp-cdr (unique-name 'tmp)]
            [tmp (unique-name 'tmp)])
            (let 
                ([val_car (if (is_fixnum? e1) e1 tmp-car)]
                [val_cdr (if (is_fixnum? e2) e2 tmp-cdr)])
                `(let ([,tmp-car ,e1] [,tmp-cdr ,e2])
                    (let ([,tmp (+ (alloc ,size-pair) ,tag-pair)])
                        (begin
                            (mset! ,tmp ,offset-car ,val_car)
                            (mset! ,tmp ,offset-cdr ,val_cdr)
                            ,tmp))))))
    ; Specify representation of a vector creation
    (define (specify_vec_create len)
        (let 
            ([tmp-len (unique-name 'tmp)]
            [tmp (unique-name 'tmp)])
            (let 
                ([val_len (if (is_fixnum? len) len tmp-len)])
                `(let ([,tmp-len ,len])
                    (let 
                        ([,tmp 
                            (+ (alloc ,(constant_folding '+ disp-vector-data val_len)) 
                                ,tag-vector)])
                        (begin
                            (mset! ,tmp ,offset-vec-len ,val_len)
                            ,tmp))))))
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
        `(mset! ,vec ,(constant_folding '+ offset-vec-data idx) ,val))
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