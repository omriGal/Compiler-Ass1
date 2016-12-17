;;; cse.scm
;;; Programmers: Omri Gal & Carmel Levy, 2016-17

(print-gensym #f)

(define exprssion '())
(define sub-exps '())
(define args '())

(define cse-2
    (lambda (exp)
        (begin
            (set! exprssion exp)
            (set! sub-exps '())
            (set! args '())
            (add-sub-exp exp)
            (filter-subexp)
            (remove-redund)
            (sort-exp)
            (generate-id sub-exps)
            (cond   ((null? sub-exps) exp)
                    ((contain-inner?)
                        (begin
                            (replace-all-inners)
                            (replace-exp)
                            (replace-exp)
                            (make-args sub-exps)
                            `(let* ,args ,exprssion)))
                    (else
                        (begin
                            (replace-exp)
                            (make-args sub-exps)
                            `(let ,args ,exprssion)))) 
            )))
            
; ( (<SubExp> <refCount>) (<SubExp> <refCount>))
(define add-sub-exp 
    (lambda (exp)
        (cond
            ((and (null? (car exp)) (not (null? (cdr exp)))) (add-sub-exp (cdr exp)))
            ((and (not (list? (car exp))) (not (null? (cdr exp)))) (add-sub-exp (cdr exp)))
            ((and (list? (car exp)) (equal? (caar exp) 'quote) (not (null? (cdr exp))))  (add-sub-exp (cdr exp)))
            ((list? (car exp))
                (begin 
                    (if (member (car exp) (map car sub-exps))
                        (increase-counter sub-exps (car exp))
                        (set! sub-exps (append (cons (list (car exp) 1) sub-exps))))
                    (add-sub-exp (car exp))
                    (if (not (null? (cdr exp)))
                            (add-sub-exp (cdr exp)))
                    )))
                    ))
                    
(define increase-counter
    (lambda (sub-list exp)
        (if (equal? exp (caar sub-list))
            (set-car! (cdar sub-list) (+ 1 (cadar sub-list)))
            (increase-counter (cdr sub-list) exp))))
            
(define filter-subexp
    (lambda ()
        (set! sub-exps (filter (lambda(x) (< 1 (cadr x))) sub-exps)))) 

; check if sub is a sub-expression of exp        
(define is-sub-exp?
    (lambda (sub exp)
        (cond   ((null? exp) #f)
                ((and (not (list? (car exp))) (null? (cdr exp))) #f)
                ((equal? sub (car exp)) #t)
                ((and (not (list? (car exp))) (not (null? (cdr exp)))) (is-sub-exp? sub (cdr exp)))
                ((list? (car exp)) 
                        (or     (is-sub-exp? sub (car exp))
                                (is-sub-exp? sub (cdr exp))))
                                )))

; checks if a sub-expression is redundent (sub-expression refCount= expression refCount)
(define is-sub-redund?
    (lambda (sub-list exp refCount)
        (cond 
            ((null? sub-list) #f)
            ((equal? exp (caar sub-list)) (is-sub-redund? (cdr sub-list) exp refCount)) ;dont check yourself
            (else
                (if (and (is-sub-exp? exp (caar sub-list) ) (= (cadar sub-list) refCount))
                    #t
                    (is-sub-redund? (cdr sub-list) exp refCount))))))

; removes all redundent sub-expressions
(define remove-redund
    (lambda()
        (set! sub-exps (filter (lambda(x) (not (is-sub-redund? sub-exps (car x) (cadr x)))) sub-exps))))

; replace the refCount with gensym
(define generate-id
    (lambda(sub-list)
        (cond
            ((null? sub-list) )
            (else
                (begin 
                    (set-cdr! (car sub-list) (gensym))
                    (generate-id (cdr sub-list)))))))
                    
;checks if a sub-expression exist in another sub-expression
(define is-inner-sub?
    (lambda (sub-list exp)
        (cond
            ((null? sub-list) #f)
            ((equal? exp (caar sub-list)) (is-inner-sub? (cdr sub-list) exp )) ;dont check yourself
            (else
                (if (is-sub-exp? exp (caar sub-list)) 
                    #t
                    (is-inner-sub? (cdr sub-list) exp))))))

;check in all sub-exps if there is a inner sub-expression
(define contain-inner?
    (lambda ()
        (ormap (lambda(x) (is-inner-sub? sub-exps (car x))) sub-exps)))

        

(define contains?
    (lambda (exp1 exp2)
        (is-sub-exp? (car exp1) (car exp2))))

(define sort-exp
    (lambda()
        (set! sub-exps (reverse-exp (sort contains? sub-exps)))))
        
(define reverse-exp
    (lambda(lst)
        (fold-right (lambda(a r) (append r (list a))) '() lst)))
                    
(define find-replace-inner
    (lambda (sub-list exp gen)
        (cond
            ((null? sub-list) #f)
            ((equal? exp (caar sub-list)) (find-replace-inner (cdr sub-list) exp gen)) ;dont check yourself
            (else
                (if (is-sub-exp? exp (caar sub-list)) 
                    (replace-inner exp gen (caar sub-list))
                    (find-replace-inner (cdr sub-list) exp gen))))))

; replace definitions in the list
(define replace-inner
    (lambda (sub gen exp)
        (cond   ((null? exp) #f)
                ((and (not (list? (car exp))) (null? (cdr exp))) #f)
                ((equal? sub (car exp)) (begin (set-car! exp gen) (replace-inner sub gen (cdr exp))))
                ((and (not (list? (car exp))) (not (null? (cdr exp)))) (replace-inner sub gen (cdr exp)))
                ((list? (car exp)) 
                        (begin  (replace-inner sub gen (car exp))
                                (replace-inner sub gen (cdr exp))))
                                )))
                                
(define replace-all-inners
    (lambda()
        (map (lambda(x) (find-replace-inner sub-exps (car x) (cdr x))) sub-exps)))
        

; replace in the expression
(define replace-exp 
    (lambda ()
        (map (lambda(x) (replace-inner (car x) (cdr x) exprssion)) sub-exps)))
        
(define make-args
    (lambda(sub-exps)
        (cond ((null? sub-exps) #f)
              (else
                (begin 
                    (set! args (cons (list (cdar sub-exps) (caar sub-exps)) args))
                    (make-args (cdr sub-exps)))))))
        
                    
                   