;;; compiler.scm
;;; Programmers: Omri Gal & Carmel Levy, 2016

(load "~/Downloads/pc.scm") ;; TODO: Change path

(define <Boolean>
  (new (*parser (word-ci "#t"))
       (*pack
        (lambda (_) #t))

       (*parser (word-ci "#f"))
       (*pack
        (lambda (_) #f))

       (*disj 2)
       done))

       
(define <CharPrefix>
  (new  (*parser (char #\#))
        (*parser (char #\\))
        (*caten 2)
  done))
  
  
(define <VisibleSimpleChar>
  (new  (*parser <any-char>)
        (*parser (range (integer->char 0) (integer->char 32)))
        *diff
        (*pack
        (lambda (ch) ch))
  done))
  

(define <NamedChar> 
(new (*parser (word-ci "lambda"))
       (*pack
        (lambda (_) 'lambda))
        
    (*parser (word-ci "newline"))
       (*pack
        (lambda (_) 'newline))
        
    (*parser (word-ci "nul"))
       (*pack
        (lambda (_) 'nul))

    (*parser (word-ci "page"))
       (*pack
        (lambda (_) 'page))
        
    (*parser (word-ci "return"))
       (*pack
        (lambda (_) 'return))
        
    (*parser (word-ci "space"))
       (*pack
        (lambda (_) 'space))
        
    (*parser (word-ci "tab"))
       (*pack
        (lambda (_) 'tab))
    
    (*disj 7)

done))

  
(define <HexChar>
  (new  (*parser (range #\0  #\9))
        (*parser (range #\a  #\f))
        (*parser (range #\A  #\F))
        (*disj 3)
        (*pack
        (lambda (ch) ch))
  done))
  
(define <HexUnicodeChar>
  (new  (*parser (char #\x))
        (*parser <HexChar>) *plus
        (*caten 2)
        
        (*pack-with
        (lambda (EX rest)
            (list->string  (cons #\x rest))))
    done))
    
(define <Char>
(new    (*parser <CharPrefix>)
        (*parser (char #\())
        
        (*parser <VisibleSimpleChar>)
        (*parser <NamedChar>)
        (*parser <HexUnicodeChar>)
        (*disj 3)
        
        (*parser (char #\)))
        
        (*caten 4)
        ; pack ???
        
        done))
        
(define <digit-0-9>
    (range #\0 #\9))

;(define <Natural>
;(let (zero (char->integer #\0))

;  (new  (*parser <digit-0-9>) *plus
        ;(*pack (lambda (dig)
        ;                (- (char->integer dig) zero))) 
                            ;(list->string   
			     ; (list num)
			     ;num)))
       ;done)))

(define <Carmel>
  (let ((zero (char->integer #\0)))
  
    (new (*parser (range #\0 #\9)) *plus
	 (*pack
	  (lambda (ch)
	    (string->number (list->string (- (char->integer ch) zero))))))

	 done)))
       
       
       
(define <Sexpr>
  (new (*parser <Boolean>)
       (*parser <Char>)
       
       (*disj 2)
       done))


