(load "lib/match.scm")
(load "lib/helpers.scm")
(load "src/expose_frame_var.scm")
(load "src/flatten_program.scm")

(define generate-x86-64
    (lambda (program)
        (let* ([expose_program (expose-frame-var program)]
                [flatten_program (flatten-program expose_program)])
            (begin
                (printf "~a\n" flatten_program)
            (match flatten_program
                [(code ,statement* ...) (codegen statement*)])))))

(define codegen
    (lambda (statements)
        (define codegen_binop
            (lambda (binop)
                (match binop
                    [+ 'addq]
                    [- 'subq]
                    [* 'imulq]
                    [logand 'andq]
                    [logor 'orq]
                    [src 'sar])))
        (define codegen_statement
            (lambda (statement)
                (match statement
                    [,label (guard (label? label)) (emit-label label)]
                    [(jump ,triv) (emit-jump 'jump triv)]
                    [(set! ,item1 (,binop ,item2 ,item3)) (emit (codegen_statement binop) item3 item1)]
                    [(set! ,item1 ,item2) (guard (or (int64? item2) (register? item2))) (emit 'movq item2 item1)])))
        (emit-program 
            (for-each codegen_statement statements))))