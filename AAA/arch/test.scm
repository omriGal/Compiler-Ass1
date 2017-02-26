
;; (define a 5)

;; (define map
;;         (lambda (f lst)
;;             (if (null? lst)
;;                 lst
;;                 (cons   (f (car lst))
;;                         (map f (cdr lst))))
;;                                             ))

'a



;; (define list 
;;     (lambda x x))
;;     
;; (define map
;;         (lambda (f lst)
;;             (if (null? lst)
;;                 lst
;;                 (cons   (f (car lst))
;;                         (map f (cdr lst))))
;;                                             ))
;;     
;; (define (append l m)
;;  (if (null? l) m
;;   (cons (car l) (append (cdr l) m))))
;; 
;; (define cddr 
;;     (lambda (x) 
;;         (cdr (cdr x))))
;; 
;; (define cdddr
;;     (lambda (x)
;;         (cdr (cddr x))))
;;         
;; (define cadddr
;;     (lambda (x)
;;         (car (cdddr x))))