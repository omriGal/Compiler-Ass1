;;; compiler.scm
;;; Programmers: Omri Gal & Carmel Levy, 2016

(load "~/Downloads/pc.scm") ;; TODO: Change path

;; From Mayar second tutorial:

(define <whitespace>
  (const
   (lambda (ch)
     (char<=? ch #\space))))

(define <line-comment>
  (let ((<end-of-line-comment>
   (new (*parser (char #\newline))
        (*parser <end-of-input>)
        (*disj 2)
        done)))
    (new (*parser (char #\;))
   
   (*parser <any-char>)
   (*parser <end-of-line-comment>)
   *diff *star

   (*parser <end-of-line-comment>)
   (*caten 3)
   done)))

(define <sexpr-comment>
  (new (*parser (word "#;"))
       (*delayed (lambda () <sexpr>))
       (*caten 2)
       done))

(define <comment>
  (disj <line-comment>
  <sexpr-comment>))

(define <skip>
  (disj <comment>
  <whitespace>))

(define ^^<wrapped>
  (lambda (<wrapper>)
    (lambda (<p>)
      (new (*parser <wrapper>)
     (*parser <p>)
     (*parser <wrapper>)
     (*caten 3)
     (*pack-with
      (lambda (_left e _right) e))
     done))))

(define ^<skipped*> (^^<wrapped> (star <skip>)))

;; Now our part:
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
  (new  
        (*parser (range #\0  #\9))
        (*parser (range #\a  #\f))
        (*parser (range #\A  #\F))
        (*disj 3)
        (*pack
        (lambda (ch) ch))
  done))
  
(define <HexUnicodeChar>
  (new  
        (*parser (char #\x))
        (*parser <HexChar>) *plus
        (*caten 2)
        
        (*pack-with
        (lambda (EX rest)
            (list->string  (cons #\x rest))))
    done))
    
(define <Char>
    (new    
        (*parser <CharPrefix>)
        
        (*parser <VisibleSimpleChar>)
        (*parser <NamedChar>)
        (*parser <HexUnicodeChar>)
        (*disj 3)
                
        (*caten 2)
        ; pack ???
        
        done))
        
(define <digit-0-9>
    (range #\0 #\9))

(define <Natural>
    (new
        (*parser <digit-0-9>)
        (*parser <digit-0-9>) *star
        (*caten 2)
        
        (*pack-with
        (lambda (first rest)
            (string->number (list->string `(,first ,@rest)))))

       done))
       
(define <Integer>
    (new
        (*parser (char #\+))
        (*parser <Natural>)
        (*caten 2)
        (*pack-with
            (lambda (plus num) num))
            
        (*parser (char #\-))
        (*parser <Natural>)
        (*caten 2)
        (*pack-with
            (lambda (minus num) 
                (- num)))
            
        (*parser <Natural>)
        
        (*disj 3)
        
    done))
    
(define <Fraction>
    (new
        (*parser <Integer>)
        (*parser (char #\/))
        (*parser <Natural>)
        (*guard (lambda(nat) (not (zero? nat))))
        (*caten 3)
        
        (*pack-with
            (lambda (int div nat) 
                (/ int nat)))
    done))
    
    
(define <Number>
    (new
        (*parser <Integer>)
        (*parser <Fraction>)
        (*disj 2)
    done))
     

(define <StringLiteralChar>
    (new
        (*parser <any-char>)
        (*parser (char #\"))
        (*parser (char #\\))
        (*disj 2)
        *diff

    done))
    

(define ^<MetaChar>
  (lambda (str ch)
    (new (*parser (word str))
         (*pack (lambda (_) ch))
   done)))   


(define <StringMetaChar>
  (new (*parser (^<MetaChar> "\\\\" #\\))
       (*parser (^<MetaChar> "\\\"" #\"))
       (*parser (^<MetaChar> "\\n" #\newline))
       (*parser (^<MetaChar> "\\r" #\return))
       (*parser (^<MetaChar> "\\t" #\tab))
       (*parser (^<MetaChar> "\\f" #\page)) 

       (*disj 6)
       done)) 


(define <StringHexChar>
  (new (*parser (char #\\))
       (*parser (char #\x))
       (*parser <HexChar>) *star
       (*parser (char #\;))
       (*caten 4)

       (*pack-with
        (lambda (a b c d)
        (integer->char
         (string->number
          (list->string `,c ) 16) )))

    done))

(define <StringChar>
  (new (*parser <StringLiteralChar>)
       (*parser <StringHexChar>)
       (*parser <StringMetaChar>)
       (*disj 3)

    done))

(define <String>
  (new (*parser (char #\"))
       (*parser <StringChar>) *star
       (*parser (char #\"))
       (*caten 3)

      (*pack-with
        (lambda (open-delim chars close-delim)
          (list->string chars)))

    done))

(define <SymbolChar>
  (new  (*parser (range #\0  #\9))
        (*parser (range #\a  #\z))
        (*parser (range #\A  #\Z))
        (*parser (char #\!))
        (*parser (char #\$))
        (*parser (char #\^))
        (*parser (char #\*))
        (*parser (char #\-))
        (*parser (char #\_))
        (*parser (char #\=))
        (*parser (char #\+))
        (*parser (char #\<))
        (*parser (char #\>))
        (*parser (char #\?))
        (*parser (char #\/))
        (*disj 15)

        (*pack 
          (lambda (sym) sym))
    done))

(define <Symbol>
  (new  (*parser <SymbolChar>) 
        (*parser <SymbolChar>) *star
        (*caten 2)

        (*pack-with
          (lambda (first rest)
            (string->symbol 
              (list->string `(,first ,@rest)))))
    done))


(define <Sexpr>
  (new 
       (*parser <Boolean>)
       (*parser <Char>)
       (*parser <Number>)
       (*parser <String>)
       (*parser <Symbol>)
       
;; <ProperList>
       (*parser (char #\( ))
       (*delayed (lambda () <Sexpr>)) *star
       (*parser (char #\) ))
       (*caten 3)
       (*pack-with 
          (lambda (open exp close) `(,@exp)))

;; <ImproperList>
       (*parser (char #\( ))
       (*delayed (lambda () <Sexpr>)) *plus
       (*parser (char #\.))
       (*delayed (lambda () <Sexpr>))
       (*parser (char #\) ))
       (*caten 5)
       (*pack-with 
          (lambda (open exp dot rest close) (list*  `,@exp `,rest)))

;; <Vector>
       (*parser (char #\# ))
       (*parser (char #\( ))
       (*delayed (lambda () <Sexpr>)) *star
       (*parser (char #\) ))
       (*caten 4)
       (*pack-with 
          (lambda (open atx exp close) `#(,@exp)))


       (*disj 8)

       done))


