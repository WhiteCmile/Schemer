(define invalid-tests
  '(5
    (set! rax 5)
    (begin 5)
    (begin (set! x 5))
    (begin (set! 5 x))
    (begin (set! (set! rax 5) 100))
    (begin (set! rax 9223372036854775808))
    (begin (set! rax -9223372036854775809))
    (begin (set! rax x))
    (begin (set! rax (set! rax 5)))
    (begin (set! rax (+ rbx rax)))
    (begin (set! rax (+ 5 rax)))
    (begin (set! rax (+ r11 100)))
    (begin (set! rax (+ rax 2147483648)))
    (begin (set! rax (+ rax -2147483649)))
    (begin (set! rax (^ rax 2)))
    (begin (set! rax (/ rax 2)))
    (begin (set! r16 (+ r16 2)))
    (begin (set! r7 (+ r7 2)))))

(define tests
  '((begin (set! rax 5))
    (begin (set! r11 5) (set! rax r11))
    (begin (set! r11 10) (set! rax -10) (set! rax (* rax r11)))
    (begin (set! r11 10) (set! r11 (* r11 -10)) (set! rax r11))
    (begin (set! rax 5) (set! rax (+ rax 10)))
    (begin (set! r8 5) (set! rax 10) (set! rax (+ rax r8)))
    (begin (set! rax 7) (set! rax (+ rax 4)))
    (begin (set! rax 7) (set! rax (- rax 4)))
    (begin (set! rax 7) (set! rax (* rax 4)))
    (begin (set! rax 5) (set! rbx -11) (set! rax (+ rax rbx)))
    (begin (set! rax 5) (set! rbx -11) (set! rax (- rax rbx)))
    (begin (set! rax 5) (set! rbx -11) (set! rax (* rax rbx)))

    ;; some tests dealing with overflow
    (begin (set! rax -9223372036854775808) (set! rax (- rax 5)))
    (begin (set! rax 9223372036854775807) (set! rax (+ rax 5)))
    (begin (set! rax 1000000000000000000) (set! rax (* rax rax)))
    (begin (set! rax 1000000000000000000) 
           (set! rbx -1)
           (set! rbx (* rbx rax))
           (set! rax (* rax rbx)))

    ;; Factorial 5 - the long way.
    (begin (set! rax 5)            
           (set! rbx 1)
           (set! rbx (* rbx rax))
           (set! rax (- rax 1))
           (set! rbx (* rbx rax))
           (set! rax (- rax 1))
           (set! rbx (* rbx rax))
           (set! rax (- rax 1))
           (set! rbx (* rbx rax))
           (set! rax rbx))))

