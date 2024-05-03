(load "lib/match.scm")
(load "lib/helpers.scm")
(load "src/expose_frame_var.scm")

(define generate-x86-64
    (lambda (program)
        (match program
            [(begin ,statement* ...) (codegen statement*)])))

(define a1_handle_binop
    (lambda (binop)
        (match binop
            [+ 'addq]
            [- 'subq]
            [* 'imulq])))

(define a1_handle_statement
    (lambda (statement)
        (match statement
            [(set! ,item1 (,binop ,item2 ,item3)) (emit (a1_handle_binop binop) item3 item1)]
            [(set! ,item1 ,item2) (guard (or (int64? item2) (register? item2))) (emit 'movq item2 item1)])))


(define codegen
    (lambda (statements)
        (emit-program 
            (for-each a1_handle_statement 
                (expose-frame-var statements)))))