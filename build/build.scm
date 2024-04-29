;;; running assembly code

(define-who build-and-run
  (define output-dir "output")
  (define file-root "t")
  (define shell
    (lambda (s . args)
      (system (apply format s args))))
  (lambda (input-expr output-string)
    (define src-file (format "~a.s" file-root))
    (define out-file (format "~a.out" file-root))
    (define exe-file (format "~a" file-root))
    (with-output-to-file src-file
      (lambda ()
        (printf "/*~%")
        (pretty-print input-expr)
        (printf "*/~%~%")
        (newline)
        (display-string output-string))
      'replace)
    (unless (= (shell "gcc -m64 -o ~a runtime.c ~a > ~a 2>&1" exe-file src-file out-file) 0)
      (printf "========\n")
      (shell "cat ~a" out-file)
      (format-error who "build error(s)"))
    (unless (= (shell "exec ./~a > ~a 2>&1" exe-file out-file) 0)
      (printf "\n========\n")
      (shell "cat ~a" out-file)
      (format-error who "run error(s)"))
   ; replace #<void> with "#<void>" to make it something the reader can
   ; handle, then substitute void for "#<void>"
    (shell "sed -e 's/#<void>/\"#<void>\"/g' < ~a > ~a.tmp" out-file out-file)
    (let ([ip (open-input-file (format "~a.tmp" out-file))])
      (let ([x (subst (void) "#<void>" (read ip))])
        (close-input-port ip)
        x))))