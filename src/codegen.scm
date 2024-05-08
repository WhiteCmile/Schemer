(load "lib/match.scm")
(load "lib/helpers.scm")
(load "src/expose_frame_var.scm")
(load "src/flatten_program.scm")
(load "src/finalize_locations.scm")
(load "src/expose_basic_blocks.scm")

(define generate-x86-64
    (lambda (program)
        (let* ([finalize_program (finalize-locations program)]
                [expose_frame_program (expose-frame-var finalize_program)]
                [expose_blocks_program (expose-basic-blocks expose_frame_program)]
                [flatten_program (flatten-program expose_blocks_program)])
            (match flatten_program
                [(code ,statement* ...) (codegen statement*)]))))

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
                    [sra 'sar])))
        (define codegen-relop
            (lambda (op invert)
                (match op
                    [= (if invert 'jne 'je)]
                    [!= (if invert 'je 'jne)]
                    [< (if invert 'jge 'jl)]
                    [<= (if invert 'jg 'jle)]
                    [> (if invert 'jle 'jg)]
                    [>= (if invert 'jl 'jge)])))
        (define codegen_statement
            (lambda (statement)
                (match statement
                    [,label (guard (label? label)) (emit-label label)]
                    [(jump ,triv) (emit-jump 'jmp triv)]
                    [(if (not (,relop ,item1 ,item2)) (jump ,triv)) (emit 'cmpq item1 item2) (emit-jump (codegen-relop relop #f) triv)]
                    [(if (,relop ,item1 ,item2) (jump ,triv)) (emit 'cmpq item1 item2) (emit-jump (codegen-relop relop #t) triv)]
                    [(set! ,item1 (,binop ,item2 ,item3)) (emit (codegen_binop binop) item3 item1)]
                    [(set! ,item1 ,item2) (guard (label? item2)) (emit 'leaq item2 item1)]
                    [(set! ,item1 ,item2) (emit 'movq item2 item1)])))
        (emit-program 
            (for-each codegen_statement statements))))