; Handle and primitive
(define-who (handle_and env expr)
    (match expr
        [(and ,[(Expr env) -> x*] ...)
            (if (null? x*)
                '(quote #t)
                (let f ([x* x*])
                    (let ([x (car x*)] [x* (cdr x*)])
                        (if (null? x*)
                            x
                            `(if ,x ,(f x*) (quote #f))))))]
        [,x (error who "Invalid and syntax ~s\n" x)]))

; Handle or primitive
(define-who (handle_or env expr)
    (match expr
        [(or ,[(Expr env) -> x*] ...)
            (if (null? x*)
                '(quote #f)
                (let f ([x* x*])
                    (let ([x (car x*)] [x* (cdr x*)])
                        (if (null? x*)
                            x
                            (let ([new_var (unique-name 'or)])
                                `(let ([,new_var ,x]) 
                                    (if ,new_var ,new_var ,(f x*))))))))]
        [,x (error who "Invalid or syntax ~s\n" x)]))

; Handle not primitive
(define-who (handle_not env expr)
    (match expr
        [(not ,[(Expr env) -> e]) `(if ,e (quote #f) (quote #t))]
        [,x (error who "Invalid not syntax ~s\n" x)]))

; convert unquoted constants into quoted constants;
; verify that each constant is well formed
; with each fixnum in the fixnum range;
(define-who (handle_constant constant)
    (match constant
        [#t '(quote #t)]
        [#f '(quote #f)]
        [() '(quote ())]
        [,n (guard (fixnum-range? n)) `(quote ,n)]
        [,x (error who "Invalid constant ~s\n" x)]))

(define-who (handle_datum datum)
    (match datum
        [#(,[handle_datum -> sub_datum*] ...)
            `(quote ,datum)]
        [(,[handle_datum -> pair_car] . ,[handle_datum -> pair_cdr])
            `(quote ,datum)]
        [,constant (handle_constant constant)]))

(define-who (handle_quote env expr)
    (match expr
        [(quote ,datum) (handle_datum datum)]
        [,x (error who "Invalid quote syntax ~s\n" x)]))

(define-who (handle_if env expr)
    (match expr
        [(if ,[(Expr env) -> pred] ,[(Expr env) -> e])
            `(if ,pred ,e)]
        [(if ,[(Expr env) -> pred] ,[(Expr env) -> conseq] ,[(Expr env) -> alt])
            `(if ,pred ,conseq ,alt)]
        [,x (error who "Invalid if syntax ~s\n" x)]))

(define-who (handle_begin env expr)
    (match expr
        [(begin ,[(Expr env) -> e*] ... ,[(Expr env) -> e])
            `(begin ,@e* ,e)]
        [,x (error who "Invalid begin syntax ~s\n" x)]))

(define-who (handle_lambda env expr)
    (match expr
        [(lambda ,var* ,e ,e* ...)
            (let-values
                ([(uvars new_env) (make_new_env env var*)])
                `(lambda ,uvars ,((Expr new_env) (make-begin `(,e ,@e*)))))]
        [,x (error who "Invalid lambda syntax ~s\n" x)]))

(define-who (handle_let env expr)
    (match expr
        [(let ([,var* ,[(Expr env) -> e*]] ...)
            ,body_e ,body_e* ...)
            (let-values
                ([(uvars new_env) (make_new_env env var*)])
                `(let ,(map list uvars e*)
                    ,((Expr new_env) (make-begin `(,body_e ,@body_e*)))))]
        [,x (error who "Invalid let syntax ~s\n" x)]))

(define-who (handle_letrec env expr)
    (match expr
        [(letrec ([,var* ,e*] ...) ,body_e ,body_e* ...)
            (let-values
                ([(uvars new_env) (make_new_env env var*)])
                `(letrec ,(map list uvars (map (Expr new_env) e*))
                    ,((Expr new_env) (make-begin `(,body_e ,@body_e*)))))]
        [,x (error who "Invalid letrec syntax ~s\n" x)]))

(define-who (handle_set! env expr)
    (match expr
        [(set! ,var ,[(Expr env) -> e]) 
            (guard (assoc var (car env)))
                `(set! ,(cdr (assoc var (car env))) ,e)]
        [,x (error who "Invalid set! syntax ~s\n" x)]))

(define-who (handle_var env expr)
    (match expr
        [(,var ,[(Expr env) -> e*] ...) `(,(cdr (assoc var (car env))) ,@e*)]
        [,x (error who "Invalid var syntax ~s with ~s operands\n" x op_num)]))

; Handle other primitives with op_num operands
(define-who (handle_prim op_num)
    (lambda (env expr)
        (match expr
            [(,prim ,e* ...)
                (guard (eq? (length e*) op_num)) 
                    `(,prim ,@(map (Expr env) e*))]
            [,x (error who "Invalid prim syntax ~s with ~s operands\n" x op_num)])))

; Make empty environment
; An environment is formed as (car . cdr) while
;   car is a list of pairs of variable and unique variable
;   cdr is a list of pairs of variable and handler
(define-who (make-empty-environment)
    (define prim1 (handle_prim 1))
    (define prim2 (handle_prim 2))
    (define prim3 (handle_prim 3))
    (list '()
        (cons 'and handle_and)
        (cons 'or handle_or)
        (cons 'not handle_not)
        (cons 'car prim1)
        (cons 'cdr prim1)
        (cons 'cons prim2)
        (cons 'make-vector prim1)
        (cons 'vector-length prim1)
        (cons 'vector-ref prim2)
        (cons 'void (handle_prim 0))
        (cons 'make-procedure prim2)
        (cons 'procedure-ref prim2)
        (cons 'procedure-code prim1)
        (cons 'boolean? prim1)
        (cons 'eq? prim2)
        (cons 'fixnum? prim1)
        (cons 'null? prim1)
        (cons 'pair? prim1)
        (cons 'vector? prim1)
        (cons 'procedure? prim1)
        (cons 'set-car! prim2)
        (cons 'set-cdr! prim2)
        (cons 'vector-set! prim3)
        (cons 'procedure-set! prim3)
        (cons '+ prim2)
        (cons '- prim2)
        (cons '* prim2)
        (cons '= prim2)
        (cons '< prim2)
        (cons '> prim2)
        (cons '<= prim2)
        (cons '>= prim2)
        (cons 'quote handle_quote)
        (cons 'if handle_if)
        (cons 'begin handle_begin)
        (cons 'lambda handle_lambda)
        (cons 'let handle_let)
        (cons 'letrec handle_letrec)
        (cons 'set! handle_set!)))

; Handling the shadowing of identifiers (other variables, keyword 
; names, and primitive names) correctly;
; Returned two values:
;   uvars: a list of unique variables
;   new_env: a new environment with unique variables
(define (make_new_env env var*)
    (let    
        ([new_uvars (map (lambda (x) (unique-name x)) var*)])
        (values new_uvars
            (cons (append (map cons var* new_uvars) (car env))
                (append (map (lambda (x) (cons x handle_var)) var*) (cdr env))))))

(define (parse-scheme program)
    (let ([env (make-empty-environment)])
        ((Expr env) program)))

; Parse Expr structure, and do following things:
;   1. verify that the syntax of the input program is correct;
;   2. verify that there are no unbound variables;
;   3. convert all variables to unique variables;
;   4. convert unquoted constants into quoted constants;
(define-who (Expr env)
    (lambda (expr)
        ; (printf "Expr: ~s\n" expr)
        (match expr
            [(,proc ,e* ...) (guard (and (not (list? proc)) 
                                    (assoc proc (cdr env))))
                ((cdr (assoc proc (cdr env))) env `(,proc ,@e*))]
            [(,[(Expr env) -> e] [(Expr env) -> e*] ...) `(,e ,@e*)]
            [,constant (guard (immediate? constant))
                (handle_constant constant)]
            [,var 
                (if (assoc var (cdr env))
                    (let ([var_uvar_pair (assoc var (car env))])
                        (if var_uvar_pair
                            (cdr var_uvar_pair)
                            var))
                    (error who "Unbound variable ~s\n" var))]
            [,x (error who "Invalid syntax ~s\n" x)])))
