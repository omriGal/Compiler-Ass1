;; (define caar 
;;     (lambda (x) 
;;         (car (car x))))
;; 
;; (define cadr 
;;     (lambda (x) 
;;         (car (cdr x))))
;;         
;; (define cdar 
;;     (lambda (x) 
;;         (cdr (car x))))
;;         
;; (define cddr 
;;     (lambda (x) 
;;         (cdr (cdr x))))
;;         
;; (define cddr 
;;     (lambda (x) 
;;         (cdr (cdr x))))
;; 
;; (define caaar
;;     (lambda (x)
;;         (car (caar x))))
;;         
;; (define caadr
;;     (lambda (x)
;;         (car (cadr x))))
;;         
;; (define cadar
;;     (lambda (x)
;;         (car (cdar x))))
;;         
;; (define caddr
;;     (lambda (x)
;;         (car (cddr x))))
;; 
;; (define cdaar
;;     (lambda (x)
;;         (cdr (caar x))))
;;         
;; (define cdadr
;;     (lambda (x)
;;         (cdr (cadr x))))
;;         
;; (define cddar
;;     (lambda (x)
;;         (cdr (cdar x))))
;;         
;; (define cdddr
;;     (lambda (x)
;;         (cdr (cddr x))))
;; 
;; (define caaaar
;;     (lambda (x)
;;         (car (caaar x))))
;;         
;; (define caaadr
;;     (lambda (x)
;;         (car (caadr x))))
;;         
;; (define caadar
;;     (lambda (x)
;;         (car (cadar x))))
;;         
;; (define caaddr
;;     (lambda (x)
;;         (car (caddr x))))
;; 
;; (define cadaar
;;     (lambda (x)
;;         (car (cdaar x))))
;;         
;; (define cadadr
;;     (lambda (x)
;;         (car (cdadr x))))
;;         
;; (define caddar
;;     (lambda (x)
;;         (car (cddar x))))
;;         
;; (define cadddr
;;     (lambda (x)
;;         (car (cdddr x))))
;; 
;; (define cdaaar
;;     (lambda (x)
;;         (cdr (caaar x))))
;;         
;; (define cdaadr
;;     (lambda (x)
;;         (cdr (caadr x))))
;;         
;; (define cdadar
;;     (lambda (x)
;;         (cdr (cadar x))))
;;         
;; (define cdaddr
;;     (lambda (x)
;;         (cdr (caddr x))))
;; 
;; (define cddaar
;;     (lambda (x)
;;         (cdr (cdaar x))))
;;         
;; (define cddadr
;;     (lambda (x)
;;         (cdr (cdadr x))))
;;         
;; (define cdddar
;;     (lambda (x)
;;         (cdr (cddar x))))
;; 
;; (define cddddr
;;     (lambda (x)
;;         (cdr (cdddr x))))
;;         

(define list 
    (lambda x x))
    
    
(define map
        (lambda (f lst)
            (if (null? lst)
                lst
                (cons   (f (car lst))
                        (map f (cdr lst))))))
          

(define (append l m)
    (if (null? l) 
        m
        (cons (car l) (append (cdr l) m))))
               
;; (define x '(5)) 
;; (define y (append '(a b) '(#t #f) '(1 2 3 4) '(5 6 7) '(8 9 -1/2) x))
;; y
;; (set-car! x #t)
;; y

;((lambda (a b c . d) ((lambda x d) "Akuna" "Matata")) -12/36 #\a '#())

((lambda x ((lambda y x))))


;((lambda (x y . z) 
;	      ((lambda (a b . c) z) 1 2 -3/4 5/6)) 12 13)
