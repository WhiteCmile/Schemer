(load "lib/helpers.scm")

(define triv?
    (lambda (triv)
        (cond
            [(int64? triv) #t]
            [(label? triv) #t]
            [(uvar? triv) #t]
            [(register? triv) #t]
            [(frame-var? triv) #t]
            [else #f])))