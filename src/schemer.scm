(load "src/verifier.scm")
(load "src/codegen.scm")

(define driver
    (lambda (program)
        (with-output-to-file "output/t.s"
            (lambda ()
                (generate-x86-64 (verify-scheme program))))))
