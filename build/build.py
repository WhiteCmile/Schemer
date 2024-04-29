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

(compiler-passes '(
  verify-scheme
  generate-x86-64
))

(load "{task}/tests{task_id}.scm")
""")
        file.flush()
    return

if __name__ == "__main__":
    main()