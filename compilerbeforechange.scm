;;; compiler.scm
;;; Programmers: Omri Gal & Carmel Levy, 2016

(case-sensitive #f)

(load "~/Downloads/pc.scm") ;; TODO: Change path

;;;;;;;;;;;; From tutorial ;;;;;;;;;;;;;;;;;;;;;

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

(define <expression-comment-prefix> (word "#;"))

(define <sexpr-comment>
  (new (*parser <expression-comment-prefix>)
       (*delayed (lambda () <Sexpr2>))
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


;;;;;;;;;;;;;;;;;;; Extended Syntax ;;;;;;;;;;;;;;;;;

(define <Boolean>
  (new 
        (*parser (word-ci "#t"))
        (*pack
            (lambda (_) #t))

        (*parser (word-ci "#f"))
        (*pack
            (lambda (_) #f))

        (*disj 2)
       done))

       
(define <CharPrefix>
  (new  
        (*parser (word "#\\"))
  done))
  

(define <NamedChar> 
(new 
        (*parser (word-ci "lambda"))
        (*pack
            (lambda (_) (integer->char 955)))
        
        (*parser (word-ci "newline"))
        (*pack
            (lambda (_) #\newline))
        
        (*parser (word-ci "nul"))
        (*pack
            (lambda (_) #\nul))

        (*parser (word-ci "page"))
        (*pack
            (lambda (_) #\page))
        
        (*parser (word-ci "return"))
        (*pack
            (lambda (_) #\return))
        
        (*parser (word-ci "space"))
        (*pack
            (lambda (_) #\space))
        
        (*parser (word-ci "tab"))
        (*pack
            (lambda (_) #\tab))
    
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
        (*guard
            (lambda (hex)
                (<  
                    (string->number
                            (list->string `,hex ) 16)
                    1114112)))
        
        (*caten 2)
        
        (*pack-with
            (lambda (pre hex)
                (integer->char
                    (string->number
                        (list->string `,hex ) 16) )))
    done))
    
(define <VisibleSimpleChar>
    (not-followed-by

    (new  
        (*parser <any-char>)
        (*parser (range (integer->char 0) (integer->char 32)))
        *diff
        (*pack
        (lambda (ch) ch))
    done)
    
    <HexChar>  
  ))
    
(define <Char>
    (new    
        (*parser <CharPrefix>)
        
        (*parser <NamedChar>)
        (*parser <HexUnicodeChar>)
        (*parser <VisibleSimpleChar>)
        (*disj 3)
                
        (*caten 2)
        
        (*pack-with
        (lambda (pre ch)
                        ch))
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
   (not-followed-by 
                
    (new
        (*parser <Fraction>)
        (*parser <Integer>)       
        (*disj 2)        
   done)

   (new
        (*parser (range #\a #\z))
        (*parser (range #\A #\Z))
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
        (*parser (char #\:))
        (*disj 15)
    done)
   
   ))
		

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
  (new 
        (*parser (^<MetaChar> "\\\\" #\\))
        (*parser (^<MetaChar> "\\\"" #\"))
        (*parser (^<MetaChar> "\\n" #\newline))
        (*parser (^<MetaChar> "\\r" #\return))
        (*parser (^<MetaChar> "\\t" #\tab))
        (*parser (^<MetaChar> "\\f" #\page)) 

        (*disj 6)
    done)) 


(define <StringHexChar>
  (new 
        (*parser (char #\\))
        (*parser (char #\x))
        (*parser <HexChar>) *plus
        (*guard
            (lambda (hex)
                (<  
                    (string->number
                            (list->string `,hex ) 16)
                    1114112)))
        
        (*parser (char #\;))
        (*caten 4)

        (*pack-with
            (lambda (sl x ch delim)
                (integer->char
                    (string->number
                        (list->string `,ch ) 16) )))
    done))

(define <StringChar>
  (new 
        (*parser <StringLiteralChar>)
        (*parser <StringHexChar>)
        (*parser <StringMetaChar>)
        (*disj 3)
    done))

(define <String>
  (new 
        (*parser (char #\"))
        (*parser <StringChar>) *star
        (*parser (char #\"))
        (*caten 3)

        (*pack-with
            (lambda (open-delim chars close-delim)
            (list->string chars)))
    done))

(define <SymbolChar>
  (new  
        (*parser (range #\0  #\9))
        (*parser (range #\a  #\z))
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
        (*parser (char #\:))
        (*disj 15)

        (*pack 
          (lambda (sym) sym))
          
        (*parser (range #\A  #\Z))
        (*pack
            (lambda (ch)
              (integer->char (+ (char->integer ch) 32)))) 
        (*disj 2)
    done))

(define <Symbol>
  (new  
        (*parser <SymbolChar>) 
        (*parser <SymbolChar>) *star
        (*caten 2)

        (*pack-with
          (lambda (first rest)
            (string->symbol 
              (list->string `(,first ,@rest)))))
    done))
    
(define <ProperList>
    (new    
        (*parser (char #\( ))
        (*delayed (lambda () <Sexpr2>)) *star
        (*parser (char #\) ))
        (*caten 3)
        (*pack-with 
          (lambda (open exp close) `(,@exp))) 
    done))
    
       
(define <ImproperList>
    (new
        (*parser (char #\( ))
        (*delayed (lambda () <Sexpr2>)) *plus
        (*parser (char #\.))
        (*delayed (lambda () <Sexpr2>))
        (*parser (char #\) ))
        (*caten 5)
        (*pack-with 
          (lambda (open exp dot rest close) 
                                `(,@exp . ,rest)))
    done))
    
(define <Vector>
    (new
        (*parser (char #\# ))
        (*parser (char #\( ))
        (*delayed (lambda () <Sexpr2>)) *star
        (*parser (char #\) ))
        (*caten 4)
        (*pack-with 
          (lambda (open atx exp close) `#(,@exp)))
    done))
    
(define <Quated>
    (new
        (*parser (char #\' ))
        (*delayed (lambda () <Sexpr2>))
        (*caten 2)
        (*pack-with
            (lambda(qu exp)
                (list 'quote exp)))
    done))
    
(define <QuasiQuated>
    (new
        (*parser (char #\` ))
        (*delayed (lambda () <Sexpr2>))
        (*caten 2)
        (*pack-with
            (lambda(qu exp)
                (list 'quasiquote exp)))
    done))
    
(define <Unquated>
    (new
        (*parser (char #\, ))
        (*delayed (lambda () <Sexpr2>))
        (*caten 2)
        (*pack-with
            (lambda(qu exp)
                (list 'unquote exp)))
    done))
    
(define <UnquateAndSpliced>
    (new 
        (*parser (char #\, ))
        (*parser (char #\@ ))
        (*delayed (lambda () <Sexpr2>))
        (*caten 3)
        (*pack-with
            (lambda(qu sh exp)
                (list 'unquote-splicing  exp)))
    done))



;--------  INFIX EXPRESSION INFRA ---------------

(define <infix-expression-comment>
  (new (*parser <expression-comment-prefix>)
       (*delayed (lambda () <InfixExpression>))
       (*caten 2)
       done))

(define <infix-comment>
  (disj <infix-expression-comment>
	<line-comment>))

(define <infix-skipped>
  (disj <whitespace>
	<infix-comment>))

(define ^<infix-skipped*>
  (^^<wrapped> (star <infix-skipped>)))

(define <InfixPrefixExtensionPrefix>
  (new  
        (*parser (word "##"))
        (*parser (word "#%"))
        (*disj 2)
  done))
  
(define <InfixParen>
    (new
        (*parser (char #\())
        (*delayed (lambda () <InfixExpression>))
        (*parser (char #\)))
        (*caten 3)
                (*pack-with (lambda (open expr close)
                                                    expr)) 
    done))
    
(define <InfixSymbolChar>
  (new  
        (*parser (range #\0  #\9))
        (*parser (range #\a  #\z))

	(*parser (one-of "!$^_=<>?"))
        (*disj 3)
        
        (*pack 
          (lambda (sym) sym))
          
        (*parser (range #\A  #\Z))
        (*pack
            (lambda (ch)
              (integer->char (+ (char->integer ch) 32)))) 
        (*disj 2)
    done))

(define <InfixSymbol>
  (new  
        (*parser <InfixSymbolChar>) 
        (*parser <InfixSymbolChar>) *star
        (*caten 2)

        (*pack-with
          (lambda (first rest)
            (string->symbol 
              (list->string `(,first ,@rest)))))
    done))
    
(define ^<op>
  (lambda (op-string op-symbol)
    (new (*parser (^<infix-skipped*> (word op-string)))
	 (*pack (lambda (_) op-symbol))
	 done)))

(define <AddSymbol> (^<op> "+" '+))
    
(define <SubSymbol> (^<op> "-" '-))
    
(define <MulSymbol>
  (not-followed-by (^<op> "*" '*)
		   (char #\*)))
    
(define <DivSymbol> (^<op> "/" '/))
    
(define <PowerSymbol>
  (disj (^<op> "^" 'expt)
	(^<op> "**" 'expt)))
    
(define <InfixNeg>
    (new 
        (*parser <SubSymbol>)
        (*delayed (lambda () <InfixCarmel>))
        (*caten 2)
         
        (*pack-with (lambda (sub expr)
                            `(- ,expr)))                  
    done))
    
(define <InfixSexprEscape>
   (new
        (*parser <InfixPrefixExtensionPrefix>)
        (*delayed (lambda () <Sexpr2>))
        (*caten 2)
        
        (*pack-with
            (lambda(extn expr)
                            expr))
        
    done))
    
(define <InfixNumber>
   (not-followed-by 
                
    (new
        (*parser <Fraction>)
        (*parser <Integer>)       
        (*disj 2)        
   done)

   (new
        (*parser (range #\a #\z))
        (*parser (range #\A #\Z))
        (*parser (char #\!))
        (*parser (char #\$))
        (*parser (char #\_))
        (*parser (char #\=))
        (*parser (char #\<))
        (*parser (char #\>))
        (*parser (char #\?))
        (*parser (char #\:))
        (*disj 10)
    done)
   
   ))
    
    
(define <InfixAtom> 
    (new 
        (*parser <InfixSexprEscape>)
        (*parser <InfixNumber>)
        (*parser <InfixParen>)
        (*parser <InfixSymbol>)
        (*parser <InfixNeg>)
        (*disj 5)
    done)) 


(define <SquareParen>
    (new
        (*parser (char #\[))
        (*delayed (lambda()  <InfixExpression>))
        (*parser (char #\]))
        (*caten 3)
        
        (*pack-with 
            (lambda (open expr close)
                            `(#f ,expr)))
    done))

(define <InfixArrayGet>
    (new
        (*parser <InfixAtom>)
        
        (*parser (star <infix-skipped>))
        
        (*parser <SquareParen>)
        *plus
        
        (*caten 3)
         
         (*pack-with (lambda (atom space paren)
                        (if (null? paren)
                            atom
                            (fold-left 
                                ( lambda (ans rest) 
                                `(vector-ref ,ans ,rest)) 
                                `(vector-ref ,atom ,(car paren))  
                                    (cdr paren))
                                             ))) 
                                             

    done))
    
    
(define <InfixArgList>
    (new
        (*delayed (lambda () <InfixExpression>))
    
        (*parser (char #\,))
        (*delayed (lambda () <InfixExpression>))
        (*caten 2)
        
        (*pack-with 
          (lambda (comma expr) expr))
        
        *star
        (*caten 2)
        
        (*pack-with 
          (lambda (expr rest) 
                    `(,expr ,@rest)))

        (*parser (star <infix-skipped>))
        (*pack (lambda (_) '()))
        (*disj 2)
    done))
    
(define <RoundParen>
    (new
        (*parser (char #\())
        (*parser <InfixArgList>)
        (*parser (char #\)))
        (*caten 3)
        
        (*pack-with 
            (lambda (open args close)
                            `(#t ,args)))
    done))
    
(define <InfixFuncall>
    (new
        (*parser <InfixAtom>)
        
        (*parser (star <infix-skipped>))
        
        (*parser <RoundParen>)
        *plus
    
        (*caten 3)
                  
        (*pack-with (lambda (atom space paren)
                        (if (null? paren)
                            atom
                            (fold-left 
                                ( lambda (ans rest) 
                                (cons `,ans `,rest)) 
                                (cons `,atom `,(car paren))  
                                    (cdr paren))
                                             ))) 
                    
                    
     ;   (*parser <InfixAtom>)
       ; (*disj 2)
    
    done))
    
(define <InfixWithParen>
    (new 
        (*parser <InfixArrayGet>)
        (*parser <InfixFuncall>)
        (*disj 2)
    done))
    
(define <InfixCarmel>
    (new
    (*parser <InfixAtom>)
    (*parser (star <infix-skipped>))
    
    (*parser <RoundParen>)
    (*parser <SquareParen>)
    (*disj 2) *plus
    
    (*caten 3)
    
     (*pack-with
     
        (lambda (atom space paren)
            (if (null? paren)
                atom
                (ParenAction atom paren))))
                
    (*parser <InfixAtom>)
    (*disj 2)
    done))
    
(define ParenAction
    (lambda (ans rest)
        (if (null? rest)
            ans
            (if (caar rest)
               (ParenAction (cons ans (cadar rest)) (cdr rest))
               (ParenAction `(vector-ref ,ans ,@(cdar rest)) (cdr rest))))))
    
(define <InfixTopRound>
    (new 
    (*parser <InfixWithParen>)
    
    (*parser (star <infix-skipped>))

    (*parser <RoundParen>)
    *plus
    
        (*caten 3)
                  
        (*pack-with (lambda (atom space  paren)
                        (if (null? paren)
                            atom
                            (fold-left 
                                ( lambda (ans rest) 
                                (cons `,ans `,rest)) 
                                (cons `,atom `,(car paren))  
                                    (cdr paren))
                                             )))
    done))

    
(define <InfixTopSquare>
    (new 
        (*parser <InfixWithParen>)
        
        (*parser (star <infix-skipped>))

        (*parser <SquareParen>)
        *plus
        
        (*caten 3)
         
         (*pack-with (lambda (atom space paren)
                        (if (null? paren)
                            atom
                            (fold-left 
                                ( lambda (ans rest) 
                                `(vector-ref ,ans ,rest)) 
                                `(vector-ref ,atom ,(car paren))  
                                    (cdr paren))
                                             ))) 
    done))
    
(define <InfixTop>
    (new
        (*parser <InfixTopRound>)
        (*parser <InfixTopSquare>)
        (*parser <InfixWithParen>)
        (*parser <InfixAtom>)
        (*disj 4)
    done))


(define <InfixPow>
  (new  
        (*parser <InfixCarmel>)

        (*parser <PowerSymbol>)
        (*delayed (lambda () <InfixPow>))
        (*caten 2)
         *star
         
        (*caten 2)

         (*pack-with (lambda (expr sym)
                        (if (null? sym)
                            expr
                            (fold-left 
                                ( lambda (ans rest) 
                                `(,(car rest) ,ans ,@(cdr rest))) 
                                `(,(caar sym) ,expr ,@(cdar sym))  
                                    (cdr sym)) ))) 
  done))
    
    
(define <InfixMulDiv>
    (new
        (*parser <InfixPow>)
        
        (*parser <MulSymbol>)
        (*parser <DivSymbol>)
        (*disj 2)

         
         (*parser <InfixPow>)
         (*caten 2)
         *star
         
         (*caten 2)
         
         (*pack-with (lambda (expr sym)
                        (if (null? sym)
                            expr
                            (fold-left 
                                ( lambda (ans rest) 
                                `(,(car rest) ,ans ,@(cdr rest))) 
                                `(,(caar sym) ,expr ,@(cdar sym))  
                                    (cdr sym)) )))
            done))

(define <InfixAddSub>
    (new
        (*parser <InfixMulDiv>)
        
        (*parser <AddSymbol>)
        (*parser <SubSymbol>)
        (*disj 2)

         
         (*parser <InfixMulDiv>)
         (*caten 2)
         *star
         
         (*caten 2)
         
         (*pack-with (lambda (expr sym)
                        (if (null? sym)
                            expr
                            (fold-left 
                                ( lambda (ans rest) 
                                `(,(car rest) ,ans ,@(cdr rest))) 
                                `(,(caar sym) ,expr ,@(cdar sym))  
                                    (cdr sym)) )))
                                            
            done))
            
(define <InfixExpression>
  (^<infix-skipped*>
   <InfixAddSub>))

(define <InfixExtension>
  (new
        (*parser <InfixPrefixExtensionPrefix>)
        (*parser <InfixExpression>)
        (*caten 2)

        (*pack-with
          (lambda (extn expr) expr))
  done))
  
  

;--------  S-EXPRESSION ---------------

(define <Sexpr2>
 (^<skipped*>
 (new 
       (*parser <Boolean>)
       (*parser <Char>)
       (*parser <Number>)
       (*parser <String>)
       (*parser <Symbol>)
       (*parser <ProperList>)
       (*parser <ImproperList>)
       (*parser <Vector>)
       (*parser <Quated>)
       (*parser <QuasiQuated>)
       (*parser <Unquated>)
       (*parser <UnquateAndSpliced>)
       (*parser <InfixExtension>)

      (*disj 13)

    done)))
            
;; (load "~/Comp/compiler.scm")
;; (load "~/Downloads/parser.so")
