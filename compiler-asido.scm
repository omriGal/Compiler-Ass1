(load "pc.scm")
(load "pattern-matcher.scm")

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

(define <infix-comment>
  (new (*parser (word "#;"))
       (*delayed (lambda () <InfixExpression>))
       (*caten 2)
       done))       

(define <expression-comment>
    (new (*parser <infix-comment>)
         (*parser <sexpr-comment>)
         (*disj 2)
         done))
       
(define <comment>
  (disj <line-comment>
	<expression-comment>))

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

(define <Boolean>
  (new (*parser (word-ci "#t"))
       (*pack (lambda (_) #t))
       (*parser (word-ci "#f"))
       (*pack (lambda (_) #f))
       (*disj 2)
       done))
       
(define <digit-0-9>
  (range #\0 #\9))
  
(define <digit-1-9>
  (range #\1 #\9))
         
(define <Natural>
  (new  (*parser (char #\0)) *plus
        (*parser <digit-1-9>)
        (*parser <digit-0-9>) *star
       
       (*caten 3)
       (*pack-with
	(lambda (zero a s)
	  (string->number
	   (list->string
	    `(,a ,@s)))))
       
       (*parser (char #\0)) *plus
       (*pack (lambda (_) 0))
       
       
       (*parser <digit-1-9>)
       (*parser <digit-0-9>) *star
       (*caten 2)
       (*pack-with
	(lambda (a s)
	  (string->number
	   (list->string
	    `(,a ,@s)))))
       
       (*disj 3)
       done))

(define <Integer>
    (new 
    
     (*parser (char #\+))
         (*parser <Natural>)
         (*caten 2)
         (*pack-with
            (lambda (plus n) n))
   
     (*parser (char #\-))
         (*parser <Natural>)
         (*caten 2)
         (*pack-with
            (lambda (minus n) (- n)))
            
         (*parser <Natural>)
          
         (*disj 3)
        done))
         
(define <Fraction>
    (new (*parser <Integer>)
        (*parser (char #\/))
        (*parser <Natural>)
       ; (*guard (lambda (n) (not (zero? n))))
        (*caten 3)
        (*pack-with
            (lambda (integer div natural) (/ integer natural)))
        done))

(define <Number>
    (new (*parser <Fraction>)
         (*parser <Integer>)
         (*delayed (lambda () <Symbol>))
         *not-followed-by
         (*disj 2)
        done))

(define <CharPrefix>
   (new (*parser (word "#\\"))
        (*pack (lambda (_) _)) 
	 done))
	 
(define ^<NamedChar>
  (lambda (str ch)
    (new (*parser (word-ci str))
	 (*pack (lambda (_) ch))
	 done)))

        
(define <NamedChar>
    (new (*parser (^<NamedChar> "page" #\page))
         (*parser (^<NamedChar> "newline" #\newline))
         (*parser (^<NamedChar> "nul" #\nul))
         (*parser (^<NamedChar> "return" #\return))
         (*parser (^<NamedChar> "space" #\space))
         (*parser (^<NamedChar> "tab" #\tab))
         (*parser (^<NamedChar> "lambda" (integer->char 955)))
         (*disj 7)
        done))
        
(define <char-33-127>
   (range (integer->char 33) (integer->char 127)))
    
    
(define <VisibleSimpleChar>
    (new (*parser  <char-33-127>)
         (*pack (lambda (ch)  ch)) 
        done))

(define <hex-a-f>
   (range (integer->char 97) (integer->char 102)))
  
(define <unicode>
    (lambda (char) (and (>= char 0) (<= char 1114111))))
  
(define <HexChar>
    (new (*parser <hex-a-f>)
         (*pack (lambda (ch) ch))
         (*parser <digit-0-9>)
         (*pack (lambda (ch) ch))
         (*disj 2)
         done))
         
(define <HexUnicodeChar>        
        (new (*parser (word "x"))
             (*parser <HexChar>) *plus
             (*caten 2)
        (*pack-with
	(lambda (a s)
	 (string->number (list->string s) 16)))
	  (*guard <unicode>)
	  (*pack integer->char)
             done))
             
(define <Char>
    (new (*parser <CharPrefix>)
    
         (*parser  <NamedChar>) 
         (*parser  <HexUnicodeChar>)
         (*parser  <VisibleSimpleChar>) 
         (*disj 3)
        
        (*caten 2)
          (*pack-with (lambda (del char) char))
        done))
        
(define <char-32-127>
   (range (integer->char 32) (integer->char 127)))
     
(define <StringLiteralChar>
    (new (*parser  <any-char>)
         (*parser  (char #\\))
         *diff
         (*pack (lambda (ch)  ch)) 
        done))
          
(define ^<meta-char>
  (lambda (str ch)
    (new (*parser (word str))
	 (*pack (lambda (_) ch))
	 done)))
	 
(define <StringMetaChar>
  (new  (*parser (^<meta-char> "\\\\" #\\))
        (*parser (^<meta-char> "\\\"" #\"))
        (*parser (^<meta-char> "\\n" #\newline))
        (*parser (^<meta-char> "\\r" #\return))
        (*parser (^<meta-char> "\\t" #\tab))
        (*parser (^<meta-char> "\\f" #\page))
        (*disj 6)
       done))

(define <StringHexChar>          
  (new  (*parser (word "\\x"))
        (*parser <HexChar>) *star
        (*parser (word ";"))
        (*caten 3)
        
        (*pack-with
	(lambda (a s d)
	  (string->number (list->string s) 16)))
	   (*guard <unicode>)
	  (*pack integer->char)
             done))      
             
(define <StringChar> 
    (new (*parser  <StringHexChar>) 
         (*parser <StringMetaChar>)
         (*parser  <StringLiteralChar>) 
         (*disj 3)
        done))  

(define <String> 
    (new (*parser  (char #\" )) 
        (*parser <StringChar>)
        (*parser (char #\"))
        *diff
        *star
        (*parser  (char #\" )) 
        (*caten 3)
         (*pack-with
          (lambda (a s d)
	     (list->string s)))
        done))  
        
(define <char-a-z>
   (range #\a #\z))
   
(define <char-A-Z>
  (new (*parser (range #\A #\Z))
        (*pack (lambda (char) (integer->char (+ (char->integer char) 32)) ))
    done))
        
(define <SymbolChar>
 (new   
        (*parser <char-a-z>)
        (*parser <char-A-Z>)
        (*parser <digit-0-9>)
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
    done))
        
(define <Symbol>
    (new (*parser <SymbolChar>) *plus
         (*pack (lambda (sym)  (string->symbol (list->string `(,@sym)))))
    done))
        
(define <ProperList> 
    (new (*parser  (char #\( )) 
           
        (*delayed (lambda () <sexpr>))
        (*parser (char #\) ))
        *diff
        *star
        (*parser  (char #\) )) 
        (*caten 3)
         (*pack-with
          (lambda (a s d)
	      s))
        done))  

(define <ImproperList> 
    (new (*parser  (char #\( )) 
        (*delayed (lambda () <sexpr>))
        (*parser (char #\) ))
        *diff
        *plus
        (*parser  (char #\. ))
        (*delayed (lambda () <sexpr>))
        (*parser (char #\) ))
        *diff
        (*parser  (char #\) )) 
        (*caten 5)
        (*pack-with
          (lambda (a b c d e)
	        `(,@b . ,d)))
        done))
        
(define <Vector> 
    (new (*parser  (char #\# ))
        (*parser  (char #\( )) 
        (*delayed (lambda () <sexpr>))
        (*parser (char #\) ))
        *diff
        *star
        (*parser  (char #\) ))
        (*caten 4)
         (*pack-with
          (lambda (a b s d)
	     (list->vector s)))
        done))
        
(define <Quoted> 
    (new (*parser  (char #\' )) 
        (*delayed (lambda () <sexpr>))        
        (*caten 2)
         (*pack-with
          (lambda (a s)
	       (list 'quote s)))     
        done))  
        
(define <QuasiQuoted> 
    (new (*parser  (char #\` )) 
        (*delayed (lambda () <sexpr>))        
        (*caten 2)
         (*pack-with
          (lambda (a s)
	       (list `quasiquote s)))
        done))  
        
(define <Unquoted> 
    (new (*parser  (char #\, )) 
        (*delayed (lambda () <sexpr>))        
        (*caten 2)
         (*pack-with
          (lambda (a s)
	       (list 'unquote s)))
        done))  
        
(define <UnquoteAndSpliced> 
    (new (*parser  (word ",@")) 
           
        (*delayed (lambda () <sexpr>))        
            
        (*caten 2)
         (*pack-with
          (lambda (a s)
	       (list 'unquote-splicing s)))
        done))
        
      
(define <InfixPrefixExtensionPrefix>
   (new (*parser (word "##"))
	(*parser (word "#%"))
        (*disj 2)	
	done))

(define <PowerSymbol>
    (new (*parser (word "**"))
         (*parser (char #\^))
         (*disj 2)
        (*pack (lambda (_)  'expt))  
      done))
      
(define <InfixSymbolChar>
 (new   (*parser <digit-0-9>)
        (*parser <char-a-z>)
        (*parser <char-A-Z>)
        (*parser (char #\!))
        (*parser (char #\$))
        (*parser (char #\_))
        (*parser (char #\=))
        (*parser (char #\<))
        (*parser (char #\>))
        (*parser (char #\?))
        (*disj 10)
    done))

(define <InfixExpression>
   (^<skipped*> (new 
         (*delayed (lambda () <InfixAddSub>)) 
        done)))      

(define <InfixSymbol>
    (new (*parser <InfixSymbolChar>) *plus
         (*pack (lambda (sym) (string->symbol (list->string `(,@sym)))))
         (*parser (word "**"))
         *diff
    done))      

(define <InfixNumber>
    (new (*parser <Fraction>)
         (*parser <Integer>)
       
         (*disj 2)
         (*parser <InfixSymbol>)
         *not-followed-by
        done)) 
 
 (define <InfixParen> 
    (new (*parser <skip>) *star 
    
         (*parser (char #\())
         (*parser <InfixExpression>)  
         (*parser (char #\)))
         
         (*caten 3)
            (*pack-with (lambda (a b c) `,b))
         (*parser <skip>) *star
         
         (*caten 3)
         
         (*pack-with (lambda (a b c) b))
       
         done))

(define <AtomExpression>
    (^<skipped*>(new 
       
         (*parser <InfixNumber>)
         (*parser <InfixSymbol>)
         (*parser <InfixParen>)
         (*parser <InfixPrefixExtensionPrefix>)
         (*delayed (lambda() <sexpr>))
         (*caten 2)
            (*pack-with (lambda (a b) `(,@b)))
         (*disj 4)
         
     done)))   
 
 (define <InfixArgList>
    (new (*parser <InfixExpression>)
         
         (*parser (char #\,))
         (*parser <InfixExpression>)
         (*caten 2)
         (*pack-with (lambda (a b) b))
         *star   
         (*caten 2)
        (*pack-with (lambda (a b) `(,a ,@b)))
                                       
        (*parser <epsilon>)
        (*disj 2)
    done))   
             
 (define <InfixArrayGetOrFuncall>
    (new 
         (*parser <skip>) *star 
         (*parser <AtomExpression>)
         
         (*parser (char #\[))  
         (*parser <InfixExpression>)
         (*parser (char #\]))
         *diff
         (*parser (char #\]))
         
         (*caten 3) 
         (*pack-with (lambda (a b c ) `(1 ,b)))
         
         (*parser <skip>) *star 
         (*parser (char #\())
         (*parser <skip>) *star 
         (*parser <InfixArgList>)
         (*parser <skip>) *star 
         (*parser (char #\)))
         (*parser <skip>) *star 
         (*caten 7) 
         (*pack-with (lambda (space1 a b c d e space2) `(0 ,@c)))
         
         (*disj 2)
         *plus
         (*parser <skip>) *star 
         (*caten 4)
            (*pack-with (lambda (space1 a b space2)  (fold-left (lambda (d x)  (if (= 1 (car x))
                                                                    `(vector-ref ,d ,@(cdr x))
                                                                     `(,d ,@(cdr x))))   
                                                    (if (= 1 (caar b))
                                                        `(vector-ref ,a ,@(cdar b))
                                                         `(,a ,@(cdar b))) (cdr b) )))
         (*parser <AtomExpression>)
         (*disj 2)
    done))
    

(define <InfixNeg>
    (new  (*parser <InfixArrayGetOrFuncall>)
          (*pack (lambda (a)  a))  
          (*parser <skip>) *star
         (*parser (char #\-))
         (*parser <skip>) *star
         (*parser <InfixArrayGetOrFuncall>)
         (*caten 4)
            
         (*pack-with 
            (lambda (a b c d)
                    `(- ,d )))
         (*disj 2)
        done))
        
(define <InfixPow>
    (new (*parser <InfixNeg>)
         (*parser  <PowerSymbol>)
         (*parser <InfixNeg>)
         (*caten 2)
            *star
            
         (*caten 2)
         (*pack-with (lambda (a b) 
                            (let ((revb (reverse b)))
                            (if (null? b)
                             a 
                             (if (= (length b) 1) 
                             `(expt ,a ,(cadar b))
                        `(expt ,a ,(fold-left (lambda (d x) `(,(car x) ,@(cdr x)  ,d )) `(,(caar revb) ,(cadadr revb) ,@(cdar revb)) (cddr revb))))))))
     done))       
     
(define <DivSymbol>
    (new (*parser (char #\/))
      done))

(define <MulDivSymbol>
    (new (*parser (char #\*))
         (*parser (^<skipped*> <DivSymbol>))
         (*disj 2)
         (*pack (lambda (sym)  (string->symbol (list->string `(,sym)))))
         
      done))
      
(define <InfixMulDiv>
    (new (*parser <InfixPow>)
         (*parser <MulDivSymbol>)       
         (*parser <InfixPow>)
         (*caten 2)
         *star
         (*caten 2)
          (*pack-with (lambda (a b)
                            (if (null? b)
                             a
                         (fold-left (lambda (d x) `(,(car x) ,d ,@(cdr x))) `(,(caar b) ,a ,@(cdar b))  (cdr b)))))

      done))
      
(define <PlusSubSymbol>
    (new (*parser (char #\+))
         (*parser (char #\-))

         (*disj 2)
         (*pack (lambda (sym)  (string->symbol (list->string `(,sym)))))
      done))
      
(define <InfixAddSub>
    (new (*parser <InfixMulDiv>)
         (*parser <PlusSubSymbol>)       
         (*parser <InfixMulDiv>)
         (*caten 2)
         *star
         (*caten 2)
         (*pack-with (lambda (a b) 
                            (if (null? b)
                             a
                         (fold-left (lambda (d x) `(,(car x) ,d ,@(cdr x))) `(,(caar b) ,a ,@(cdar b))  (cdr b)))))
        done))


(define <InfixExtension>
    (new (*parser <InfixPrefixExtensionPrefix>)        
         (*parser <InfixExpression>)
         (*caten 2)
        (*pack-with (lambda (a b) b)) 
        done))   

(define <sexpr>
(^<skipped*>
  (new  (*parser <Boolean>)
        (*parser <Char>)
        (*parser <Number>)
        (*parser <String>)
        (*parser <Symbol>)
        (*parser <ProperList>)
        (*parser <ImproperList>)
        (*parser <Vector>)
        (*parser <Quoted>)
        (*parser <QuasiQuoted>)
        (*parser <Unquoted>)
        (*parser <UnquoteAndSpliced>)
        (*parser <InfixExtension>)
        (*disj 13)
       done)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 


(define ^quote?
  (lambda (tag)
    (lambda (e)
      (and (pair? e)
            (eq? (car e) tag)
            (pair? (cdr e))
            (null? (cddr e))))))

(define quote? (^quote? 'quote))
(define unquote? (^quote? 'unquote))
(define unquote-splicing? (^quote? 'unquote-splicing))

(define const?
  (let ((simple-sexprs-predicates
            (list boolean? char? number? string?)))
    (lambda (e)
      (or (ormap (lambda (p?) (p? e)) simple-sexprs-predicates)
        (quote? e)))))

(define quotify
  (lambda (e)
    (if (or (null? e)
	    (pair? e)
	    (symbol? e)
	    (vector? e))
	`',e
	e)))

(define unquotify
  (lambda (e)
    (if (quote? e)
	(cadr e)
	e)))

(define const-pair?
  (lambda (e)
    (and (quote? e)
	 (pair? (cadr e)))))

(define expand-qq
  (letrec ((expand-qq
	    (lambda (e)
	      (cond ((unquote? e) (cadr e))
		    ((unquote-splicing? e)
		     (error 'expand-qq
		       "unquote-splicing here makes no sense!"))
		    ((pair? e)
		     (let ((a (car e))
			   (b (cdr e)))
		       (cond ((unquote-splicing? a)
			      `(append ,(cadr a) ,(expand-qq b)))
			     ((unquote-splicing? b)
			      `(cons ,(expand-qq a) ,(cadr b)))
			     (else `(cons ,(expand-qq a) ,(expand-qq b))))))
		    ((vector? e) `(list->vector ,(expand-qq (vector->list e))))
		    ((or (null? e) (symbol? e)) `',e)
		    (else e))))
	   (optimize-qq-expansion (lambda (e) (optimizer e (lambda () e))))
	   (optimizer
	    (compose-patterns
	     (pattern-rule
	      `(append ,(? 'e) '())
	      (lambda (e) (optimize-qq-expansion e)))
	     (pattern-rule
	      `(append ,(? 'c1 const-pair?) (cons ,(? 'c2 const?) ,(? 'e)))
	      (lambda (c1 c2 e)
		(let ((c (quotify `(,@(unquotify c1) ,(unquotify c2))))
		      (e (optimize-qq-expansion e)))
		  (optimize-qq-expansion `(append ,c ,e)))))
	     (pattern-rule
	      `(append ,(? 'c1 const-pair?) ,(? 'c2 const-pair?))
	      (lambda (c1 c2)
		(let ((c (quotify (append (unquotify c1) (unquotify c2)))))
		  c)))
	     (pattern-rule
	      `(append ,(? 'e1) ,(? 'e2))
	      (lambda (e1 e2)
		(let ((e1 (optimize-qq-expansion e1))
		      (e2 (optimize-qq-expansion e2)))
		  `(append ,e1 ,e2))))
	     (pattern-rule
	      `(cons ,(? 'c1 const?) (cons ,(? 'c2 const?) ,(? 'e)))
	      (lambda (c1 c2 e)
		(let ((c (quotify (list (unquotify c1) (unquotify c2))))
		      (e (optimize-qq-expansion e)))
		  (optimize-qq-expansion `(append ,c ,e)))))
	     (pattern-rule
	      `(cons ,(? 'e1) ,(? 'e2))
	      (lambda (e1 e2)
		(let ((e1 (optimize-qq-expansion e1))
		      (e2 (optimize-qq-expansion e2)))
		  (if (and (const? e1) (const? e2))
		      (quotify (cons (unquotify e1) (unquotify e2)))
		      `(cons ,e1 ,e2))))))))
    (lambda (e)
      (optimize-qq-expansion
       (expand-qq e)))))

(define identify-lambda
	(lambda (argl ret-simple ret-opt ret-var)
		(cond 
			((null? argl) (ret-simple '()))
			((var? argl) (ret-var argl))     
			(else (identify-lambda (cdr argl)
					(lambda (s) (ret-simple `(,(car argl) ,@s))) 
					(lambda (s opt) (ret-opt `(,(car argl) ,@s) opt)) 
					(lambda (var) (ret-opt `(,(car argl)) var)))))))
					
(define *reserved-words*
    '(and begin cond define do else if lambda
    let let* letrec or quasiquote unquote
    unquote-splicing quote set!))

(define beginify
	(lambda (s)
		(cond
			((null? s) *void-object*)
			((null? (cdr s)) (car s))
			(else `(begin ,@s)))))
			
(define  simple-const?
    (lambda (c)
        (or (null? c)
            (vector? c)
            (boolean? c)
            (char? c)
            (number? c)
            (string? c)
        )))
        
(define argsduplicate?
        (lambda (args)
            (cond ((null? args) #t)
                   ((memq (car args) (cdr args)) #f)
                    (else (argsduplicate? (cdr args))))))    

(define args?
    (lambda (args)
        (if (andmap var? args) 
            #t
             (error 'parse
                (format "I can't recognize this: ~s" args)))
        ))
                
(define argsdup?
    (lambda (args)
        (identify-lambda
            args
            (lambda (s) (and (args? s) (argsduplicate? s)))
            (lambda (s opt) (and (args? (cons opt s)) (argsduplicate? (cons opt s))))
            (lambda (s) (var? s)))))                  
                  

(define condleg?
    (lambda (exps)
        (andmap (lambda (x) (> (length x) 1)) exps)
        ))
        
(define  var?
    (lambda (c)
        (and (symbol? c)
             (not (memq c *reserved-words*)) 
        )))
        
(define void?
      (lambda (c)
       (eq? (void) c)
       ))

(define *void* (void))

(define notlist?
    (lambda (arg)
        (not (list? arg))
        ))
(define applic?
    (lambda (lst)
        (and (list? lst) (not (member (car lst) *reserved-words*)))
    ))
(define build-if
    (lambda (lst)
    (if (null? (cdr lst))
        (car lst)
        `(if ,(car lst) ,(build-if (cdr lst)) ,#f))))

(define build-cond
    (lambda (lst)
    (if (null? (cdr lst))
            (cond ((eq? 'else (caar lst)) (beginify (cdar lst)))
                  (else `(if ,(caar lst) ,(beginify (cdar lst))) ))
        (cond ((eq? 'else (caar lst)) (beginify (cdar lst))) 
            (else `(if ,(caar lst) ,(beginify (cdar lst)) ,(build-cond  (cdr lst))))))))
        
(define clean-begin
  (lambda (lst)
    (fold-right (lambda (x d)
                    (if (list? x)
                      (if (eq? (car x) 'begin)
                          (clean-begin (append (cdr x) d))
                          (cons x d))
                      (cons x d))) 
                      '() lst)))

                
(define clean-seq
  (lambda (lst)
    (fold-right (lambda (x d)
              (if (eq? (car x) 'seq)
                          (append (clean-seq (cdr x)) d)
                          (if (list? (car x))
                            (append x d)
                            (cons x d))))
                '() lst)))
                
(define parse
    (let ((run 
        (compose-patterns
            (pattern-rule
                (? 'c void?)
                (lambda (c)  `(const ,*void*)))
            (pattern-rule
                (? 'c simple-const?)
                (lambda (c)  `(const ,c)))
            (pattern-rule
                `(quote ,(? 'c))
                (lambda (c)  `(const ,c)))
            (pattern-rule
                (? 'v var?)
                (lambda (v) `(var ,v)))
            (pattern-rule
                `(if ,(? 'test) ,(? 'dit))
                (lambda (test dit)  `(if3 ,(parse test) ,(parse dit) ,(parse (void)))))
            (pattern-rule
                `(if ,(? 'test) ,(? 'dit) ,(? 'dif))
                (lambda (test dit dif) `(if3 ,(parse test) ,(parse dit) ,(parse dif))))
                
            (pattern-rule
                `(or . ,(? 'exps) )
                (lambda (exps) 
                        (if (null? exps)
                            `(const #f)
                            (if (= (length exps) 1)
                             (parse (car exps)) 
                            `(or  ,(map parse exps))))))
          
          ;;lambda
            (pattern-rule
               `(lambda ,(? 'args argsdup?) ,(? 'e1) . ,(? 'rest-body) )
                (lambda (args e1 rest-body)  
                    
                  `(,@(identify-lambda  args
                    (lambda (s) `(lambda-simple ,s)) 
                    (lambda (s opt) `(lambda-opt ,s ,opt)) 
                    (lambda (var) `(lambda-var ,var))),(if (null? rest-body)
                                                       (parse e1) 
                                                       (parse `(begin ,e1 ,@rest-body)) ))))
           ;;begin
            (pattern-rule
                `(begin . ,(? 'body))
                (lambda (body)
                       
                        (if (null? body)
                            (parse (void))
                            (if (= (length body) 1)
                                (parse (car body))
                        `(seq (,@(map parse (clean-begin body)))) ))))
           
           
           ;;define
            (pattern-rule
                `(define ,(? 'var var?) . ,(? 'exp))
                (lambda (var exp) (if (= 1 (length  exp))
                                 `(def ,(parse var) ,(parse  (car exp))) 
                                 `(def ,(parse var) ,(parse  `(begin ,@exp))))))
            (pattern-rule
                `(define  (,(? 'var ) . ,(? 'args)) ,(? 'e1) . ,(? 'rest-body))
                (lambda (var args e1 rest-body) `(def ,(parse  var) ,(parse `(lambda ,args  ,e1 ,@rest-body) )) ))
            
            ;;applic
            (pattern-rule 
                `(,@(? 'lst applic?) ) 
                (lambda (lst)  `(applic ,(parse (car lst)) (,@(map parse (cdr lst))) )))
            
            ;;let
            (pattern-rule
              `(let () ,(? 'expr) . ,(? 'exprs list?))
               (lambda (expr exprs) (parse `((lambda ()  (begin ,expr ,@exprs))))))

            (pattern-rule 
                `(let ,(? 'args) ,(? 'body) . ,(? 'rest-body)) 
                 (lambda (args body rest-body)
                    (let ((keys (map car args))
                          (vals  (map cadr args)))
                   (parse `((lambda ,keys ,body ,@rest-body) ,@vals)))))
           
           ;;let*
          (pattern-rule
            `(let* () ,(? 'expr) . ,(? 'exprs list?))
	     (lambda (expr exprs) (parse `((lambda () (begin ,expr ,@exprs))))))
           
           (pattern-rule
                `(let* ((,(? 'var var?) ,(? 'val)) . ,(? 'rest)) . ,(? 'exprs))
                (lambda (var val rest exprs) (parse `(let ((,var ,val)) ,(if (null? rest) (append (list 'begin) exprs) `(let* ,rest . ,exprs))))))
          
          ;;letrec 
         (pattern-rule `(letrec ,(? 'args) ,(? 'e1) . ,(? 'rest-body))
                        (lambda (args e1 rest-body)
                          (let ((keys (map car args))
                                (vals  (map cadr args)))
           (parse `(let (,@(map (lambda (key) (list key #f)) keys))
                                      ,@(map (lambda (key val) `(set! ,key ,val)) keys vals) ((lambda () ,e1 ,@rest-body)))))))
          
          ;;set!
          (pattern-rule `(set! ,(? 'setvar var?) ,(? 'setval))
                        (lambda (setvar setval) `(set ,(parse setvar) ,(parse setval))))
          
          ;;and
          (pattern-rule 
              `(and . ,(? 'exps))
                 (lambda (exps)
                        (cond
                            ((null? exps) `(const #t))
                            ((= (length exps) 1) (parse (car exps)))
                            (else (parse (build-if exps))))))   
         
         ;;cond
            (pattern-rule 
              `(cond . ,(? 'exps condleg?))
                 (lambda (exps)
                             (parse (build-cond exps))))                    
         
         ;;qq
           (pattern-rule 
            `   (quasiquote . ,(? 'exps ))
            (lambda (exps) (parse (expand-qq (car exps)))))
            )))
              
            (lambda (e)
                (run e
                (lambda ()
                (error 'parse
                (format "I can't recognize this: ~s" e)))))))

                
                
(define nested-define
  (lambda (pes ret-ds-es)
    (if (null? pes) (ret-ds-es '() '())
        (nested-define
         (cdr pes)
         (lambda (ds es)
           (cond ((eq? (caar pes) 'def) (ret-ds-es (cons (car pes) ds) es))
                 ((eq? (caar pes) 'seq)
                  (nested-define
                   (cadar pes) (lambda (ds1 es1) (ret-ds-es (append ds1 ds) (append es1 es)))))
                 (else (ret-ds-es ds (cons (car pes) es)))))))))       
             
(define base-nested
    (lambda (ds es)
            (if (null? ds) (eliminate-nested-defines es)
            (let ((args (map (lambda (x) `(,@(cadadr x))) ds))
                  (vals (map (lambda (x) `(set ,(cadr x) ,@(eliminate-nested-defines (cddr x)))) ds))
                  (false-list (fold-left (lambda (x d) (cons `(const #f) x)) '() ds)))
                  
                  `((applic (lambda-simple ,args (seq (,@vals ,@(eliminate-nested-defines es)))) ,false-list)))
                  )))
                  
(define inside-define?
  (lambda (pes)
    (nested-define pes (lambda (ds es) (not (null? ds))))))
                  
                  
(define eliminate-nested-defines
  (lambda (pes)
     (cond ((null? pes) pes)
         
         
         ((and (list? pes) (equal? (car pes) 'lambda-simple)) 
         (if (inside-define? (cddr pes)) 
         `(lambda-simple ,(cadr pes) ,@(nested-define (cddr pes)  base-nested))
                `(lambda-simple ,(cadr pes) ,@(eliminate-nested-defines (cddr pes)))))
         
         ((and (list? pes) (equal? (car pes) 'lambda-opt)) 
          (if (inside-define? (cdddr pes)) 
          `(lambda-opt ,(cadr pes) ,(caddr pes) ,@(nested-define (cdddr pes)  base-nested)) 
                `(lambda-opt ,(cadr pes) ,(caddr pes) ,@(eliminate-nested-defines (cdddr pes)))))
         
         ((and (list? pes) (equal? (car pes) 'lambda-var)) 
         (if (inside-define? (cddr pes)) 
         `(lambda-var ,(cadr pes) ,@(nested-define (cddr pes)  base-nested))
                `(lambda-var ,(cadr pes) ,@(eliminate-nested-defines (cddr pes)))))
                
           ((list? pes) (map eliminate-nested-defines pes))
           (else pes))
           ))  
           
(define remove-applic-lambda-nil
  (lambda (expr)
    (cond ((null? expr) expr)
          ((and (list? expr) (equal? 'applic (car expr)) (and (list? (cadr expr)) (equal?  'lambda-simple (caadr expr))) (null? (cadadr expr)) 
          (null? (caddr expr))) (remove-applic-lambda-nil (car (cddadr expr))))
          ((list? expr) (map remove-applic-lambda-nil expr))
          (else expr))
          )) 
          
; --------------------------------------------------------------------

(define set?
  (lambda (param body)
    (cond ((null? body) #f)
           ((and (list? body) (equal? (car body) 'lambda-simple) (member param (cadr body))) #f)  
           ((and (list? body) (equal? (car body) 'lambda-opt)  (member param (append (cadr body) (list (caddr body))))) #f)
           ((and (list? body) (equal? (car body) 'lambda-var)  (member param (list (cadr body)))) #f)

          ((and (list? body) (equal? 'set (car body)) (equal? `(var ,param) (cadr body))) #t)
          ((list? body) (ormap (lambda (x) (set? param x)) body))
          (else #f))))          
                
          
(define get?
    (lambda (param body)
        (cond ((null? body) #f)
              ((and (list? body) (equal? (car body) 'lambda-simple) (member param (cadr body))) #f)  
              ((and (list? body) (equal? (car body) 'lambda-opt)  (member param (append (cadr body) (list (caddr body))))) #f)
              ((and (list? body) (equal? (car body) 'lambda-var)  (member param (list (cadr body)))) #f)
              ((and (list? body) (equal? `(var ,param) body)) #t)
              ((and (list? body) (equal? 'set (car body)) (equal? `(var ,param) (cadr body))) (get? param (cddr body)))
              ((list? body) (ormap (lambda (x) (get? param x)) body))
             (else #f))
           ))
          
(define bounded-member
    (lambda (param env)
        (cond 
            ((null? env) #f)
            ((member param (car env)) #t)
            (else (bounded-member param (cdr env))))
            )) 
           
(define bound?
    (lambda (param params body env)
        (cond
         ((null? body) #f)
         ((and (list? body) (equal? (car body) 'lambda-simple) (not (member param (cadr body)))) (bound? param (cadr body) (cddr body) (cons params env)))
         ((and (list? body) (equal? (car body) 'lambda-simple) (member param (cadr body))) #f)
        
         ((and (list? body) (equal? (car body) 'lambda-opt) (not (member param (append (cadr body) (caddr body))))) (bound? param 
                        (append (cadr body) (caddr body)) (cdddr body) (cons params env)))
         ((and (list? body) (equal? (car body) 'lambda-opt)  (member param (append (cadr body) (list (caddr body))))) #f)
    
         ((and (list? body) (equal? (car body) 'lambda-var) (not (member param (list (cadr body))))) (bound? param (cadr body) (cddr body) (cons params env)))
          ((and (list? body) (equal? (car body) 'lambda-var)  (member param (list (cadr body)))) #f)
         
         ((and (equal? `(var ,param) body) (bounded-member param env)) #t)
         ((list? body) (ormap (lambda (x) (bound? param params x env)) body)) 
         
         (else #f))
        
        ))
 
(define shouldbebox
    (lambda (body env params)
       (lambda (param)
              (and (get? param body) (set? param body) (bound? param params body env)))
              ))  
    
(define replacebodyparam
    (lambda (param body)
            (cond
                ((null? body) body)
                ((and (list? body) (equal? (car body) 'lambda-simple) (member param (cadr body))) body)  
              ((and (list? body) (equal? (car body) 'lambda-opt)  (member param (append (cadr body) (list (caddr body))))) body)
              ((and (list? body) (equal? (car body) 'lambda-var)  (member param (list (cadr body)))) body)
                ((and (list? body) (equal? 'var (car body)) (equal? param (cadr body))) `(box-get (var ,(cadr body)) ,@(replacebodyparam param (cddr body))))
                ((and (list? body) (equal? 'set (car body)) (equal? param (cadadr body))) `(box-set (var ,(cadadr body)) ,@(replacebodyparam param (cddr body))))
                ((list? body) (map (lambda (x) (replacebodyparam param x)) body))
                (else body))
    ))

(define replacebody
    (lambda (params body)
       (if (null? params)
            (box-set body)
           (replacebody (cdr params) (replacebodyparam (car params) body)))
       ))

       
(define box-set
    (lambda (pes)
        (cond ((null? pes) pes)
              ((and (list? pes) (equal? (car pes) 'lambda-simple)) 
              (let ((box-args (filter (shouldbebox (cddr pes) '() (cadr pes)) (cadr pes))))
              
                  (if (null? box-args) `(lambda-simple ,(cadr pes) ,@(box-set (cddr pes)))   
                                  (let  ((add-set  (map (lambda(param) `(set (var ,param) (box (var ,param)))) box-args))
                                         (replace-body (clean-seq (replacebody box-args (cddr pes)))))
             `(lambda-simple (,@(cadr pes)) (seq  (,@add-set ,@replace-body)))))))
             
             ((and (list? pes) (equal? (car pes) 'lambda-opt)) 
              (let ((box-args (filter (shouldbebox (cdddr pes) '() (cadr pes)) (append (cadr pes) (list (caddr pes))))))
                  (if (null? box-args) `(lambda-opt ,(cadr pes) ,(caddr pes) ,@(box-set (cdddr pes)))   
                                  (let  ((add-set  (map (lambda(param) `(set (var ,param) (box (var ,param)))) box-args))
                                         (replace-body (clean-seq (replacebody box-args (cdddr pes)))))
              `(lambda-opt (,@(cadr pes)) ,(caddr pes) (seq  (,@add-set ,@replace-body)))))))
             
             ((and (list? pes) (equal? (car pes) 'lambda-var)) 
              (let ((box-args (filter (shouldbebox (cddr pes) '() (list (cadr pes))) (list (cadr pes)))))
                  (if (null? box-args) `(lambda-var ,(cadr pes) ,@(box-set (cddr pes)))   
                                  (let  ((add-set  (map (lambda(param) `(set (var ,param) (box (var ,param)))) box-args))
                                         (replace-body (clean-seq (replacebody box-args (cddr pes)))))
           `(lambda-var ,(cadr pes) (seq  (,@add-set ,@replace-body)))))))
             ((list? pes) (my-map box-set pes))
             (else pes)
             )
             ))       

(define my-map
  (lambda (proc items)
    (if (null? items)
        (list)
        (cons (proc (car items))
              (my-map proc (cdr items))))))             
             
(define make-minor
    (lambda (param params)
       (cond
            ((null? params) (error #f "")) 
            ((equal? param (car params)) 0)
            (else (+ 1 (make-minor param (cdr params)))))
            ))
        
(define make-bounded
    (lambda (param env)
        (cond
            ((null? env) (error #f ""))
            ((member param (car env))  (list 0 (make-minor param (car env))))
            (else (let ((find-bvar (make-bounded param (cdr env))))
                    (cons (add1 (car find-bvar)) (cdr find-bvar)))))
                    ))
    
(define lexcial-var
    (lambda (param params env)
      (cond 
            ((member param params) `(pvar ,param ,(make-minor param params)))
            ((bounded-member param env) `(bvar ,@(cons param (make-bounded param env))))
            (else `(fvar ,param)))
            ))

(define make-lexical
    (lambda (pes params env)
        (cond 
            ((null? pes) pes)
            ((and (list? pes) (equal? (car pes) 'var)) (lexcial-var (cadr pes) params env))
            
            ((and (list? pes) (equal? (car pes) 'lambda-simple))
                (let ((args (cadr pes))
                      (body (caddr pes)))
                    `(lambda-simple ,args ,(make-lexical body args (cons params env)))))
             ((and (list? pes) (equal? (car pes) 'lambda-opt))
                (let ((args (cadr pes))
                      (rest (caddr pes))  
                      (body (cadddr pes)))
                      `(lambda-opt ,args ,rest ,(make-lexical body (append args (list rest)) (cons params env)))))
              ((and (list? pes) (equal? (car pes) 'lambda-var))
                (let ((args (cadr pes))
                      (body (caddr pes)))
                      `(lambda-var ,args ,(make-lexical body (list args) (cons params env)))))
             
             ((list? pes) (my-map (lambda (x) (make-lexical x params env)) pes))

            (else pes))
            ))
    
(define pe->lex-pe
    (lambda (pes)
        (make-lexical pes '() '())
        ))

        
(define annotate-last-special
    (lambda (pes tp?)
        (let* 
             ((last (list-tail pes (- (length pes) 1)))
              (firsts (reverse (cdr (reverse pes) ))))
               (append (map annotate-tc firsts ) (annotate last tp?)))
        ))        

(define annotate
    (lambda (pes tp?)
        (cond
            ((null? pes) pes)
            ((and (list? pes) (or  (member (car pes) '(var fvar pvar bvar)) (equal? (car pes) 'const))) pes)
            ((and (list? pes) (member (car pes) '(or seq))) `(,(car pes) ,(annotate-last-special (cadr pes) tp?)))
            ((and (list? pes) (equal? (car pes) 'if3)) `(if3 ,(annotate (cadr pes) #f) ,(annotate (caddr pes) tp?) ,(annotate (cadddr pes) tp?)))
            ((and (list? pes) (member (car pes) '(def set box-set))) `(,(car pes) ,(cadr pes) ,@(annotate (cddr pes) #f)))
            ((and (list? pes) (equal? (car pes) 'box-get) `(,(car pes) ,(cadr pes) ,@(annotate (cddr pes) #f))))
            ((and (list? pes) (equal? (car pes) 'lambda-simple)) `(lambda-simple ,(cadr pes) ,@(annotate (cddr pes) #t)))
            ((and (list? pes) (equal? (car pes) 'lambda-opt)) `(lambda-opt ,(cadr pes) ,(caddr pes)  ,@(annotate (cdddr pes) #t)))
            ((and (list? pes) (equal? (car pes) 'lambda-var)) `(lambda-var ,(cadr pes) ,@(annotate (cddr pes) #t)))
            ((and (list? pes) (equal? (car pes) 'applic)) (if tp? `(tc-applic ,@(annotate (cdr pes) #f)) `(applic ,@(annotate (cdr pes) #f))))
            ((list? pes) `(,(annotate (car pes) tp?) ,@(annotate (cdr pes) tp?)))
            (else pes))
    ))
        
(define annotate-tc 
    (lambda (pes)
        (annotate pes #f)
        ))