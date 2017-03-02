(load "remww.scm")

(define display-green-with-number
    (lambda (counter)
        (display (format 
            (string-append
                "\033[1;32m"
                "===================== All ("
                (number->string counter)
                (if (= counter 1)
                    ") TEST"
                    ") TESTS" )
                    " SUCCEEDED =====================\033[0m \n")))))
                    
(define display-wrong-test
    (lambda(x)
        (display (format "\033[1;31m==================== WRONG TEST ===================="))(display (format "\033[1;39m"))(newline)(newline)
        (display (format "\033[0;31mTest:"))(display (format "\033[1;39m"))(newline)(display (car x))(newline)(newline)
        (display (format "\033[0;31mExpected:"))(display (format "\033[1;39m"))(newline)(display (car (cdr x)))(newline)(newline)
        (display (format "\033[0;31mActual:"))(display (format "\033[1;39m"))(newline)(display (car (cdr (cdr x))))(newline)(newline)))

(define bad-summary
    (lambda()
        (let*
            ((wrong-count
                (length (filter (lambda(x) (not (equal? #t x))) (test-func tests mayer-results))))
             (succsess-count
                (- (length tests) wrong-count)))
            (display (format 
                (string-append
                    "\033[1;32m"
                    "==================== "
                    (number->string succsess-count)
                    " / "
                    (number->string (+ wrong-count succsess-count))
                    (if (= succsess-count 1)
                        " TEST"
                        " TESTS" )
                    " SUCCEEDED ====================\033[0m \n")))
            (newline)
            (display (format 
                (string-append
                    "\033[1;31m"
                    "====================== "
                    (number->string wrong-count)
                                        " / "
                    (number->string (+ wrong-count succsess-count))
                    (if (= wrong-count 1)
                        " TEST"
                        " TESTS" )
                    " FAILED ======================\033[0m \n"))))))

(define test-all
    (lambda ()
        (if (null? (filter (lambda(x) (not (equal? #t x))) (test-func tests mayer-results)))
            ((lambda ()
                (display-green-with-number (length tests))))
            (begin
                (map
                    display-wrong-test
                    (filter (lambda(x) (not (equal? #t x))) (test-func tests mayer-results)))
                (bad-summary)
                (void)))))

(define test-func
    (lambda (lst r)
        (if (null? lst)
            '()
            (cons (if (equal?   (remww (car lst))
                                (car r))
                        #t
                        (list   (car lst)
                                (car r)
                                    (remww (car lst))))
                    (test-func (cdr lst) (cdr r))))))
                    
(define tests
    (list
        ;;test1
        '(  (g46494 (6 1) (6))
            (g46495 (3 6 0) (0 4))
            (g46496 (1) (3))
            (g46497 (5 0 6) (4 2))
            (g46498 (7 1 3) ())
            (g46499 (7) ())
            (g46500 (0 1) ())
            (g46501 (4 7) (7))
            (g46502 (5 7 0) ())
            (g46503 (5 2 0) (6 2))
            (g46504 (6 4 5) (0))
            (g46505 (1 2) (1))
            (g46506 (6 2 7 5) (7))
            (g46507 (1) ())
            (g46508 (1) ())
            (g46509 (0 3 2) (5 0))
            (g46510 (3 6) (4)))

        ;;test2
        '(  (g46494 (6 1) (9))
            (g46495 (3 6 0) (0 4))
            (g46496 (1) (3))
            (g46497 (9) (4 2))
            (g46498 (7 1 3) (9)) 
            (g46499 (7) (9)))

        ;;test3
        '(  (inst1 (1) (1))
            (isnt2 (1) (1))
            (inst3 () (1)))
        
        ;;test4
        '(  (inst1 () (1))
            (inst2 () (1)))

        ;;test5
        '(  (inst1 () (1)))

        ;;test6
        '(  (g46494 (6 1) (6))
            (g46494 (6 1) (6))
            (g46494 (6 1) (6))
            (g46494 (6 1) (6))
            (g46494 (6 1) (6))
            (g46494 (7) (6)))
            
        ;;test7
        '(
            (g46494 (5 1) (6))
            (g46494 (6 1 2) (6))
            (g46494 (1 1) (2))
            (g46494 (1 1) (6))
            (g46494 (6 2) (2)))
                    
        
))

(define mayer-results
    (list
    
        ;;test1
        '(  (g46494 (6 1) (6))
            (g46495 (3 6 0) (0 4))
            (g46496 (1) (3))
            (g46497 (5 0 6) (4 2))
            (g46501 (4 7) (7))
            (g46503 (5 2 0) (6 2))
            (g46504 (6 4 5) (0))
            (g46505 (1 2) (1))
            (g46506 (6 2 7 5) (7))
            (g46509 (0 3 2) (5 0))
            (g46510 (3 6) (4)))
            
        ;;test2
        '(
            (g46494 (6 1) (9))
            (g46495 (3 6 0) (0 4))
            (g46496 (1) (3))
            (g46497 (9) (4 2))
            (g46499 (7) (9)))
            
        ;;test3
        '(  (inst3 () (1)))
        
        ;;test4
        '(  (inst2 () (1)))
        
        ;;test5
        '(  (inst1 () (1)))
        
        ;;test6
        '(  (g46494 (7) (6)))
        
        ;;test7
        '(
            (g46494 (1 1) (2))
            (g46494 (1 1) (6))
            (g46494 (6 2) (2)))
))


(define test-all2
    (test-all)

    )



      
  








