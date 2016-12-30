;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;; Comp171 - ASS2 - CSE - Tests

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print-gensym #f)
; Change to your own location
(load "cse.scm")
(load "cse.so")

(define my-parse-func cse-2)
(define staff-parse-func cse)

(define try-catch
  (lambda (try-thunk catch-thunk)
    (guard (c (else (catch-thunk)))
     (try-thunk))))

(define a (lambda args 1))
(define b (lambda args 2))
(define c (lambda args 3))
(define foo (lambda args 3))
(define goo (lambda args 13))
(define f (lambda args 5))
(define g (lambda args 6))
(define h (lambda args 7))
(define func (lambda args (lambda args func)))
(define x 5)
(define y 12)
(define z 18)

(define eval-input
  (lambda (cse input)
     (eval (cse input))
))    

(define replace-gensym
  (lambda (exp-lst)
    (if (null? exp) '()
    (map (lambda (el) 
	    (cond 	    	    
	    ((gensym? el) (symbol->string el))
	    ((not (pair? el)) el)
	    ((list? el) (replace-gensym el))
	    (else (append (replace-gensym (car exp-lst)) (replace-gensym (cdr exp-lst)))))) exp-lst))
))

(define replace-gensym-to-string
  (lambda (exp-lst)
    (if (null? exp) '()
    (map (lambda (el) 
	    (cond 	    	    
	    ((gensym? el) "g")
	    ((not (pair? el)) el)
	    ((list? el) (replace-gensym-to-string el))
	    (else (append (replace-gensym-to-string (car exp-lst)) (replace-gensym-to-string (cdr exp-lst)))))) exp-lst))
))

(define equal-let-vars?
  (lambda (staff-res my-res)
    (let ((staff-vars (map cadr (cadr staff-res)))
	  (my-vars (map cadr (cadr my-res))))
	  (andmap (lambda (x) (member x staff-vars)) my-vars))

))	  

(define verify-equality
  (lambda (input)      
      (let* ((my-res-with-str (begin (gensym-count 0) (replace-gensym-to-string (my-parse-func input))))
	     (staff-res-with-str (begin (gensym-count 0) (replace-gensym-to-string (staff-parse-func input)))))	     
      (and 
	(equal? (car staff-res-with-str) (car my-res-with-str))
	(equal? (length (cadr staff-res-with-str)) (length (cadr my-res-with-str)))
	(equal? (length (cddr staff-res-with-str)) (length (cddr my-res-with-str)))
	(if (and (list? staff-res-with-str) (or (equal? (car staff-res-with-str) 'let) (equal? (car staff-res-with-str) 'let*)))
		(and (equal? (cddr my-res-with-str) (cddr staff-res-with-str)) 
		     (equal-let-vars? my-res-with-str staff-res-with-str)) #t)
	(equal? (eval-input staff-parse-func input) (eval-input my-parse-func input))))
))    

(define testVSstaff
	(lambda (input)
		(begin (display input)
		(let* ((my-res (begin (gensym-count 0) (replace-gensym (my-parse-func input))))
		      (staff-res (begin (gensym-count 0) (replace-gensym (staff-parse-func input)))))			
			(display (format "\n => ~s\n" my-res))
			(try-catch
			  (lambda ()
			    (cond ((or (equal? staff-res my-res) (verify-equality input))
				    (display (format "\033[1;32m Success! ☺ \033[0m \n")) #t)
				    (else 
				    (display (format "\033[1;31m Failed! ☹\033[0m , Expected: ~s, Actual: ~s \n" staff-res my-res)) #f)))
			  (lambda () (display (format "\n\033[1;34mUNABLE TO DETERMINE SUCESS/FAILURE!\nPLEASE CHECK MANUALLY THE INPUT: ~s\033[0m\n" input)) #f))
			))))
			
			
(define runTests
  (lambda (tests-name lst)
	(newline)
	(display (format "\033[1m~s" tests-name))
	(display ":")
	(newline)
	(display "================\033[0m")
	(newline)
	(let ((results (map testVSstaff lst)))
	(newline)
	(cond ((andmap (lambda (exp) (equal? exp #t)) results)	
		(display (format "\033[1;32m~s Tests: SUCCESS! ☺ \033[0m\n \n" tests-name)) #t)		
		(else
		(display (format "\033[1;31m~s Tests: FAILED! ☹ \033[0m\n \n" tests-name)) #f)))
))

(define runAllTests
  (lambda (lst)
    (let ((results (map (lambda (test) (runTests (car test) (cdr test))) lst)))
      	(cond ((andmap (lambda (exp) (equal? exp #t)) results)		
		(display "\033[1;32m !!!!!  ☺  ALL TESTS SUCCEEDED  ☺  !!!!\033[0m\n"))
		(else (display "\033[1;31m #####  ☹  SOME TESTS FAILED  ☹  #####\033[0m\n")))
		(newline))
))

(define quotedListsTests
  (list 
      (cse '(append '(a b c d e) '(a b c d e) '(g f h) '(a b c d e) '(a b c d e) '(a b c d e) '(g f h)))
      (cse '(g (f '('(1 2 3 4 5 6 7 8 9 0) '(a b c d e)) (list f g h) '('(1 2 3 4 5 6 7 8 9 0) '(a b c d e))) (list f g h)))
      (cse '(list '(a b) (list '(a b) '(c d)) (list '(a b) '(c d))))
      (cse '(f (+ x 1) (f x) (g x) (f (f x)) (+ x 1)))
      (cse '(begin '(a b) '(a b)))       
))

(define otherTests
  (list
    (cse '(* (+ 2 (f 3 5) 4) (+ 2 (f 3 5) 4))   )
    (cse '(+ (+ 1 2 3) (+ 4 5 6) (+ 1 2 3) (+ 4 5 6)))
    (cse '(a (a 1) (b 2) (a 1) (b 2) (c 3) (c 3)))
    (cse '(f (c (a b)) (a b) (c (a b))))
    (cse '(f (c (a b)) (a b) (c (a b)) (a b))  )  
    (cse '(foo (a b b b b b b)))
    (cse '(foo (a (b b) (b c) (b b) (b c) (b b) (b c))))
    (cse '(begin (a) (a) (b) (b) (b) (c) (c) (c) (c)))
    (cse '(foo (a) (a) (b) (b) (b) (b) (c) (c) (c)))
    (cse '(foo (a) (b) (c) (b) (c) (b) (c) (a)))
    (cse '(begin (define goo (a (b b) (b c) (b b) (b c) (b b) (b c))) (a b)))
    (cse '(a (f (+ (g) (h)) 1 (g (+ (g) (h)) (+ (g) (h))) 3 (g (+ (g) (h)) (+ (g) (h))) (+ (g) (h)))))
    (cse '(f '('(+ x 1)) (f x) (g x) (f (f x)) '(+ x 1)))
    (cse '(begin '(a b) '(a b))  )   
    
    (cse '(+ (+ (+ x 2) 1) (+ (+ x 2) 1) (+ (+ x 2) 1) (+ (+ x 2) 1)) )
    (cse '(let ((a (+ x 1)) (b (+ x 1)))
      (let ((c (+ x 1)) (d (+ x 1)))
       (* a b c d))))
    (cse '(((((((((((func x 1)))))))))) ((((((((((func x 1))))))))))))
    (cse '(list (list (list + 2 1)) (list (list + 2 1))))
    (cse '(* (+ (+ 1 (+ 2 (- 3 (+ 4 5))))) (+ (+ 1 (+ 2 (- 3 (+ 4 5)))))))
    (cse '(* (+ (* 1 (+ 2 (- 3 (+ 4 5))))) (+ (* 6 (+ 7 (- 8 (+ 4 5))))) (+ (* 9 (+ 10 (- 11 (+ 4 5))))) (+ (* 12 (+ 13 (- 14 (+ 4 5)))))))
    (cse '(* (+ (+ 1 (+ 2 (- 3 (+ 4 5))))) (+ (+ 1 (+ 2 (- 3 (+ 4 5))))) (+ (+ 11 (+ 22 (- 45 (+ 4 5)))))
	(+ (+ 113 (+ 220 (- 3 (+ 4 5)))))))
    (cse '(let ((a (+ 2 1)) (b (+ 3 (+ 2 1)))) (+ b a)))
    (cse '(or (and) (and) (or 1 2 3) (or) (or (or) (or))))
    (cse '(+ (- (*) (+)) (*) (+) (*) (+ (*) (+))))
    (cse '(+ (- (+ (* (+ (+ (+) (f) (*) (g) (+) (g) (*) 4 (+))))))))
    (cse '((+ (1) 2) (+ (1) 2)))
    (cse '(let ((a 5)) a))
    (cse '(let () a))
    (cse '(let* () a))
    (cse '(letrec () a))
    (cse '(a))
    (cse '(+ 5 2 5 2 5 2 5 2))
    (cse '(lambda () (begin (+ 5 2) (+ 4 5) (+ 5 2) (+ 4 5))))
    
    ; Shachar Cfir Tests:
    (cse '((+ 1 (- 2 3)) (+ 1 (- 2 3)) (- 5 (- 2 3)) (- 5 (- 2 3)) 89))
    (cse '((+ 1 (- 2 3) (- 2 3)) (+ 1 (- 2 3) (- 2 3))))
    ))

(define mayerExamplesTests
  (list  
    (cse '(+ 2 3))
    (cse '(f (f (f (f x)))))
    (cse '(* (+ 2 3 4) (+ 2 3 4)))
    (cse '(f (g x y) (f (g x y) z)) )      
    (cse '(+ (* (- x y) (* x x)) (* x x) (foo (- x y)) (goo (* (- x y) (* x x)))))
    (cse '(f (g x) (g (g x)) (h (g (g x)) (g x)) ((g x) (g x))))
    (cse '(list (cons 'a 'b) (cons 'a 'b) (list (cons 'a 'b) (cons 'a 'b)) (list (list (cons 'a 'b) (cons 'a 'b)))))
    (cse '(list '(a b) (list '(a b) '(c d)) (list '(a b) '(c d))))
))

(display (format "\033[1mComp171 - CSE Tests\033[0m\n====================\n"))

(runAllTests
  (list
      (cons "Mayer Examples" mayerExamplesTests)     
      (cons "Quoted Lists" quotedListsTests)   
      (cons "Other Tests" otherTests)       
))