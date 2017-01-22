;;; remww.scm
;;; Programmers: Omri Gal & Carmel Levy, 2016-17

(define remww
    (lambda (commands)
        (let (  (ans '())
                (rev-exp (get-true-lists commands)) )                                                    
              (go-over rev-exp ans)              
              )))

(define get-true-lists
    (lambda (commands)
        (map (lambda(com) (cons com (make-list (length (caddr com)) #t))) (reverse-exp commands))))
        
(define reverse-exp
    (lambda (exp)
        (fold-right (lambda(a r) (append r (list a))) '() exp)))
        
(define true-member?
    (lambda (lst)
        (member #t lst)))
        
(define go-over
    (lambda (exp ans)
            (cond   ((null? exp) ans)
                    ((true-member? (cdar exp)) 
                        (begin
                            (find-writes    (get-writes exp) (cdr exp))
                            (find-reads     (get-reads exp) (cdr exp))
                            (go-over (cdr exp) (cons (caar exp) ans))))
                    
                    (else 
                           (go-over (cdr exp)  ans)))
                ))
                
(define get-writes
    (lambda (exp)
      (car (cddaar exp))))
      
(define get-reads
    (lambda (exp)
      (cadaar exp)))

;; if num is member of write, turns the corresponding bool to #f
(define mark-false
    (lambda (num writes bools)
        (cond   ((null? writes) #f)
                ((equal? num (car writes)) (set-car! bools #f))
                (else (mark-false num (cdr writes) (cdr bools))))))

;; for every previous expression
(define find-writes-1
    (lambda (num rest-exp)
        (cond   ((null? rest-exp) #f)
                (else 
                    (begin    
                        (mark-false num (get-writes rest-exp) (cdar rest-exp))
                        (find-writes-1 num (cdr rest-exp)))))))

;; for every member of writes                        
(define find-writes            
    (lambda (writes rest-exp)
        (cond   ((null? writes) #f)
                (else
                    (begin
                            (find-writes-1 (car writes) rest-exp)
                            (find-writes (cdr writes) rest-exp))))))

                            
;; if num is member of read, turns the corresponding bool to #t
(define mark-true
    (lambda (num writes bools)
        (cond   ((null? writes) #f)
                ((equal? num (car writes)) (set-car! bools #t))
                (else (mark-true num (cdr writes) (cdr bools))))))
                
;; until the first read occur in write
(define find-reads-1
    (lambda (num rest-exp)
        (cond   ((null? rest-exp) #f)
                ((member num (get-writes rest-exp)) (mark-true num (get-writes rest-exp) (cdar rest-exp)))
                (else 
                    (find-reads-1 num (cdr rest-exp))))))
                
;; for every member of reads                        
(define find-reads            
    (lambda (reads rest-exp)
        (cond   ((null? reads) #f)
                (else
                    (begin
                            (find-reads-1 (car reads) rest-exp)
                            (find-reads (cdr reads) rest-exp))))))
                
    
    
    
    
;(remww '((g1 (2) (1 2)) (g2 (5) (1 2 3)) (g3 (8) (4)) (g4 (6) (4 5))))
        
; (g123 () (1 2)) aada

; '((g1 () (1)) (g2 (5) (1 2)) (g3 (8) (1)))

;(((g3 (8) (1)) #f) ((g2 (5) (1 2)) #f #f) ((g1 () (1)) #f))