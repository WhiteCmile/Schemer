(load "lib/match.scm")

(define introduce-allocation-forms
    (lambda (program)
        (match program
            [(letrec ([,label* (lambda () ,[body*])] ...) ,[body])
                `(letrec ([,label* (lambda () ,body*)] ...) ,body)]
            [(locals ,uvar* ,true_body)
                `(locals ,uvar*
                    (ulocals ()
                        (locate () ,true_body)))])))
