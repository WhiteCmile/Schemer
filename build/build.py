import sys
import os

def main():
    arguments = sys.argv[1 : ]
    with open("test.scm", "w") as file:
        task = arguments[0]
        task_id = task[task.index("a") + 1 : ]
        file.write(
f"""
(eval-when (compile load eval)
  (optimize-level 2)
  (case-sensitive #t)
)

(load "lib/match.scm")
(load "lib/helpers.scm")
(load "build/fmts.pretty")
(load "build/driver.scm")
(load "build/build.scm")

(load "src/schemer.scm")
(load "{task}/{task}-wrapper.scm")

(define-who everybody-home?
    (define all-home?
        (lambda (body)
            (match body
                [(locals (,local* ...)
                    (ulocals (,ulocals* ...)
                        (spills (,spill* ...)
                            (locate (,home* ...)
                                (frame-conflict, ct ,tail))))) #f]
                [(locate (,home* ...) ,tail) #t]
                [,x (error who "invalid Body ~s" x)])))
    (lambda (x)
        (match x
            [(letrec ([,label* (lambda () ,body*)] ...) ,body)
                (andmap all-home? `(,body ,body* ...))]
            [,x (error who "invalid Program ~s" x)])))

(compiler-passes '(
  verify-scheme
  remove-complex-opera*
  flatten-set!
  impose-calling-conventions
  uncover-frame-conflict
  pre-assign-frame
  assign-new-frame
  (iterate
    finalize-frame-locations
    select-instructions
    uncover-register-conflict
    assign-registers
    (break when everybody-home?)
    assign-frame)
  discard-call-live
  finalize-locations
  expose-frame-var
  expose-basic-blocks
  flatten-program
  generate-x86-64
))

(load "{task}/tests{task_id}.scm")
""")
        file.flush()
    return

if __name__ == "__main__":
    main()
