;; ((((lambda (x)
;;      (lambda (y)
;;        (x y)))
;;    (lambda (p)
;;      (p (lambda (x y)
;;  (lambda (p)
;;    (p y x))))))
;;   (lambda (z) (z #t #f)))
;;  (lambda (x y) x)) 
 
 
;; (((lambda (x)
;;      (lambda (y)
;;        (x +  y ))) (lambda (y z)
;;                     (y z))       ) 3)
;;                     
;;                     (((lambda (x)


;; ((((lambda (x)
;;      (lambda (y)
;;        (x y)))
;;    (lambda (p)
;;      (p (lambda (x y)
;;  (lambda (p)
;;    (p y x))))))
;;   (lambda (z) (z #t #f)))
;;  (lambda (x y) x)) 
 
;; (define test
;;   (let ((p1 (lambda (x1 x2 x3 x4 x5 x6 x7 x8 x9 x10)
;;      (lambda (z)
;; (z x2 x3 x4 x5 x6 x7 x8 x9 x10 x1))))
;; (s '(a b c d e f g h i j)))
;;     (lambda ()
;;       (((((((((((apply p1 s) p1) p1) p1) p1) p1) p1) p1) p1) p1)
;;       list))))
;; 
;; (test) 

;; (define test
;;   (let ((p1 (lambda (x1)
;;      (lambda (z)
;; (z  x1))))
;; (s '(a )))
;;     (lambda ()
;;       ((apply p1 s)))))
;; 
;; (test) 
;;  
 
;;  ((lambda (x)
;;     (x 1 2)) (lambda(z y) z))
 
 
;; (define chuck
;; 
;; ((lambda (x)
;;      (lambda (y)
;;        (x y)))
;;    
;;    (lambda (p)
;;      (p (lambda (x y)
;;  (lambda (p)
;;    (p y x))))))  )
;;    
;;    ((chuck
;;   (lambda (z) (z #t #f)))
;;  (lambda (x y) x)) 