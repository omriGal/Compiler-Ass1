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


(define <Sexpr>
  (new (*parser <boolean>)
       (*parser <NamedChar>)
       (*disj 2)
       done))


