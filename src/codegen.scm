(load "lib/match.scm")
(load "src/lift_letrec.scm")
(load "src/normalize_context.scm")
(load "src/specify_representation.scm")
(load "src/uncover_locals.scm")
(load "src/remove_let.scm")
(load "src/remove_complex_opera.scm")
(load "src/flatten_set.scm")
(load "src/expose_allocation_pointer.scm")
(load "src/impose_calling_conventions.scm")
(load "src/uncover_conflict.scm")
(load "src/assign_frame.scm")
(load "src/finalize_locations.scm")
(load "src/select_instructions.scm")
(load "src/assign_registers.scm")
(load "src/discard_call_live.scm")
(load "src/expose_memory_operands.scm")
(load "src/expose_frame_var.scm")
(load "src/expose_basic_blocks.scm")
(load "src/optimize_jumps.scm")
(load "src/flatten_program.scm")
(load "src/uncover_free.scm")
(load "src/convert_closures.scm")
(load "src/introduce_proc_prims.scm")
(load "src/optimize_direct_call.scm")
(load "src/remove_anonymous_lambda.scm")
(load "src/sanitize_binding_forms.scm")
(load "src/optimize_known_call.scm")

(define generate-x86-64
    (lambda (program)
        (match program
            [(code ,statement* ...) (codegen statement*)])))

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
                    [(if (not (,relop ,item1 ,item2)) (jump ,triv)) (emit 'cmpq item2 item1) (emit-jump (codegen-relop relop #t) triv)]
                    [(if (,relop ,item1 ,item2) (jump ,triv)) (emit 'cmpq item2 item1) (emit-jump (codegen-relop relop #f) triv)]
                    [(set! ,item1 (,binop ,item2 ,item3)) (emit (codegen_binop binop) item3 item1)]
                    [(set! ,item1 ,item2) (guard (label? item2)) (emit 'leaq item2 item1)]
                    [(set! ,item1 ,item2) (emit 'movq item2 item1)])))
        (emit-program 
            (for-each codegen_statement statements))))