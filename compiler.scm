;;; compiler.scm
;;; Programmers: Omri Gal & Carmel Levy, 2016-17

(case-sensitive #f)

(load "pc.scm")
(load "pattern-matcher.scm")
(load "qq.scm")

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
        (*delayed (lambda () <InfixFunArray>))
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

    

    
(define <InfixFunArray>
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


(define <InfixPow>
  (new  
        (*parser <InfixFunArray>)

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
    
;--------  TAG-PARSER ---------------
            
(define *reserved-words*
    '(and begin cond define do else if lambda
        let let* letrec or quasiquote unquote
        unquote-splicing quote set!))
        
(define reserved-word? 
    (lambda (arg)
             (member arg *reserved-words*)))
        
(define *void-object* (if #f #f))

(define notNull?
    (lambda(x) (not (null? x))))

(define notNull-2?
    (lambda (a b) 
        (notNull? a)))
        
(define constant?
        (lambda (arg)
            (or 
                (null? (null? arg))
                (vector? arg)
                (boolean? arg)
                (char? arg)
                (number? arg)
                (string? arg))))
        
(define variable?
        (lambda (arg)
            (and (symbol? arg)
                (not (reserved-word? arg)))))
           
(define hasDup?
        (lambda (lst)
            (cond
                 ((null? lst) #f)
                 ((member (car lst) (cdr lst)) #t)
                (else (hasDup? (cdr lst))))))

(define (flatten x)
  (cond ((null? x) '())
        ((pair? x) (append (flatten (car x)) (flatten (cdr x))))
        (else (list x))))                
                
(define isLambdaParams?
        (lambda (arg) 
              (cond 
                 ((null? arg) #t)
                 ((variable? arg) #t)
                 ((and (pair? arg) (andmap variable? (flatten arg)) (not (hasDup? (flatten arg)))) #t)
                (else (error 'parser (format "Invalid parameter list: ~s" arg))))))
                
                    
(define isArgsDefine?
        (lambda (arg) 
            (or (list? arg) (pair? arg))))

;; Verify let args are valid: (<var> <value>)
(define isLetDef?
        (lambda(arg)
            (cond
                ((not (list? arg)) #f)
                ((null? arg) #t)
                ((and 
                    (list? (car arg)) 
                    (= (length (car arg)) 2) 
                    (variable? (caar arg)))
                                    (isLetDef? (cdr arg)))
                (else #f))))

;; Arranges the let defines in 2 lists: ( (<vars>) (<values>) )
(define arrangeLetVarValue
    (lambda (expr vars values)
        (if
            (null? expr)
            (cons vars values)
            (arrangeLetVarValue (cdr expr) 
                                (append vars (list (caar expr))) 
                                (append values (list (cadar expr)))))))

(define setLetrecArgs
    (lambda (vars values ans)
        (if (null? vars) ans
            (setLetrecArgs (cdr vars) 
                            (cdr values)
                            (append ans `( (set! ,(car vars) ,(car values)) ))))))

(define expand-cond
    (lambda (condition arg rest-args . rest)
            (cond ((null? rest) (append `(if ,condition (begin ,arg ,@rest-args))))
                  ((eq? condition 'else) (append `(begin ,arg ,@rest-args)))
                  ((eq? (caar rest) 'else) (append `(if ,condition (begin ,arg ,@rest-args) ,@(cdar rest))))
                  (else (a `(if ,condition (begin ,arg ,@rest-args) (cond ,@rest)))))))

(define splicing-begin 
    (lambda (exps)
        (letrec (
                (inner-search (lambda (func ans args) 
                                (if (null? args) 
                                    ans 
                                    (func (car args) (inner-search func ans (cdr args))))))
                (inner-splice (lambda (condition rest)
                                (if (equal? (car condition) 'seq)
                                        (append (splicing-begin (cdr condition)) rest)
                                        (if (list? (car condition))
                                                (append condition rest)
                                                (cons condition rest))))))
            (inner-search inner-splice '() exps))))
        
;--------  PARSE ---------------

(define parse
    (let ((run 
            (compose-patterns
                    
                    (pattern-rule
                        (? 'c constant?)
                        (lambda (c) 
                            `(const ,c)))
                    
                    (pattern-rule
                        `(quote ,(? 'c))
                        (lambda (c) 
                            `(const ,c)))
                    
                    (pattern-rule
                        (? 'v variable?)
                        (lambda (v) 
                            `(var ,v)))
                    ;if rule
                    (pattern-rule
                        `(if ,(? 'test) ,(? 'dit) ,(? 'dif))
                        (lambda (test dit dif) 
                            `(if3 ,(parse test) ,(parse dit) ,(parse dif))))                    
                    
                    (pattern-rule
                        `(if ,(? 'test) ,(? 'dit))
                        (lambda (test dit) 
                            `(if3 ,(parse test) ,(parse dit) (const ,*void-object*))))
                    
                    ;or rule 
                    (pattern-rule
                        `(or . ,(? 'or-exps))
                        (lambda (or-exps) 
                            (cond   
                                ((null? or-exps) (parse #f))
                                ((equal? (length or-exps) 1) (parse (car or-exps)))
                                (else
                                    `(or ,(map parse or-exps))))))
                     
                    ;lambda rule 
                    (pattern-rule
                        `(lambda ,(? 'argl isLambdaParams?) . ,(? 'exp1 notNull?))
                        (lambda (argl exp1)
                        `(
                            ,@(identify-lambda argl 
                                    (lambda(s) `(lambda-simple ,s))
                                    (lambda(s opt) `(lambda-opt ,s ,opt))
                                    (lambda (var) `(lambda-var ,var)))
                            
                            ,(parse `(begin ,@exp1))
                                    )))
                                    
                    ;define rule 
                    (pattern-rule
                        `(define ,(? 'varArg variable?) ,(? 'expr notNull?))
                            (lambda (varArg expr)
                                `(def (var ,varArg) ,(parse expr))))
                                
                    ;MIT-define rule
                    (pattern-rule
                        `(define ,(? 'varArg isArgsDefine?) ,(? 'exp1) . ,(? 'expRest))
                            (lambda (varArg exp1 expRest)
                                `(def (var ,(car varArg)) ,(parse `(lambda ,(cdr varArg) ,exp1 . ,expRest)))))
                                
                    ;Assignmnet rule
                    (pattern-rule
                        `(set! ,(? 'varArg variable?) ,(? 'exp) )
                            (lambda (varArg exp)
                                `(set ,(parse varArg) ,(parse exp))))
                                
                    ;Applications rule
                    (pattern-rule
                        `( ,(? 'func (lambda(x) (not (reserved-word? x)))) . ,(? 'rest list?))
                            (lambda (func rest)
                                `(applic ,(parse func) ,(map parse rest))))
                       
                    ;Begin rule   
                    (pattern-rule
                        `(begin . ,(? 'begin-exps))
                         (lambda (begin-exps) 
                            (cond   
                                ((null? begin-exps) `(const ,*void-object*))
                                ((equal? (length begin-exps) 1) (parse (car begin-exps)))
                                (else
                                   `(seq ,(splicing-begin (map parse begin-exps) ))))))

                    ;let rule
                    (pattern-rule
                        `(let ,(? 'let-def isLetDef?) . ,(? 'let-exp notNull?))
                         (lambda (let-def let-exp)
                            (let ((var-values (arrangeLetVarValue let-def '() '())))
                                (parse `( (lambda ,(car var-values) ,@let-exp) 
                                                 ,@(cdr var-values))))))
                    
                    ;letrec rule
                    (pattern-rule
                        `(letrec ,(? 'let-def isLetDef?) . ,(? 'let-exp notNull?))
                         (lambda (let-def let-exp)
                            (let* ((var-values (arrangeLetVarValue let-def '() '()))
                                  (set-values (setLetrecArgs (car var-values) (cdr var-values) '()))
                                  (false-list (make-list (length (car var-values)) #f)))
                            (parse `((lambda ,(car var-values) 
                                                ,@set-values
                                                ((lambda () ,@let-exp)))
                                    ,@false-list)))))    
                                    
                    ;let* rule
                    
                    ;with arguments
            	    (pattern-rule
                        `(let* ((,(? 'var variable?) ,(? 'value)) . ,(? 'rest)) . ,(? 'exp notNull?))
                         (lambda (var value rest exp) 
                            (parse `(let ((,var ,value))
                                            ,(if (null? rest)
                                            (cons 'begin exp)
                                           `(let* ,rest . ,exp))))))
                                        
                    ;Empty arguments
                    (pattern-rule 
                        `(let* () ,(? 'exp) . ,(? 'rest list?))
                         (lambda (exp rest) 
                                    (parse `((lambda () (begin ,exp ,@rest))))))

                    ;Quasi-quate rule
                    (pattern-rule 
                        `(,'quasiquote ,(? 'exp)) 
                         (lambda (x) (parse (expand-qq x))))  
                        
                    ;And rule
                    (pattern-rule `(and . ,(? 'exps))
                        (lambda (exps)
                            (letrec ((expand-and (lambda (and-exp)
                                                        (cond 
                                                            ((null? and-exp) #t)
                                                            ((equal? (length and-exp) 1) (car and-exp))
                                                            (else `(if ,(car and-exp) ,(expand-and (cdr and-exp)) #f))))))
                                (parse (expand-and exps)))))         
                                                            
                    ;Cond rule                    
                    (pattern-rule `(cond (,(? 'condition) ,(? 'arg) . ,(? 'rest-args)) . ,(? 'rest))
                        (lambda (condition arg rest-args rest)
                                    (cond   ((eq? condition 'else) (parse `(begin ,arg ,@rest-args)))
                                            ((null? rest) (parse `(if ,condition (begin ,arg ,@rest-args))))
                                            ((eq? (caar rest) 'else) (parse `(if ,condition (begin ,arg ,@rest-args) (begin ,@(cdar rest)))))
                                            (else (parse `(if ,condition (begin ,arg ,@rest-args) (cond ,@rest)))))))            
            
                    )))                            
                    
                    (lambda (sexpr)
                            (run sexpr
                                (lambda () (error 'parser
                                                    (format "Unknown form: ~s" sexpr)))))))
                                                        
(define identify-lambda
    (lambda (argl ret-simple ret-opt ret-var)
        (cond   ((null? argl) (ret-simple '()))
                ((symbol? argl) (ret-var argl))
                (else (identify-lambda (cdr argl)
                        (lambda (s) (ret-simple `(,(car argl) ,@s)))
                        (lambda (s opt) (ret-opt `(,(car argl) ,@s) opt))
                        (lambda (var) (ret-opt `(,(car argl)) var )))))))
            

            
; ------------------------------------------------------------------
; ----------------------- Part 3  ----------------------------------

(define inner-eliminate-nested-defines
    (lambda (parsed-exp ret-ds+es)
        (if  (null? parsed-exp) (ret-ds+es '() '())     
               (inner-eliminate-nested-defines (cdr parsed-exp)
                                      (lambda (ds es)   
                                            (cond ((eq? (caar parsed-exp) 'def) 
                                                        (ret-ds+es (cons (car parsed-exp) ds) es))
                                                  ((eq? (caar parsed-exp) 'seq) 
                                                  (inner-eliminate-nested-defines (cadar parsed-exp)
                                                                            (lambda (ds1 es1) 
                                                                                (ret-ds+es
                                                                                (append ds1 ds)
                                                                                (append es1 es)))))
                                            (else (ret-ds+es ds (cons (car parsed-exp) es)))))))))
(define eliminate-nested-defines
  (lambda (exp)
    (cond ((null? exp) exp)
          ((and (list? exp) (ormap (lambda (x) (eq? (car exp) x)) '(lambda-simple lambda-var)))
            (cond ((inner-eliminate-nested-defines (get-lambda-body exp) notNull-2?) 
                        `(,(car exp) ,(get-lambda-args exp) ,@(inner-eliminate-nested-defines (get-lambda-body exp) 
                                                                                (lambda (x y)
                                                                                    (let* ((make-args (map (lambda (exp) `(,@(cadadr exp))) x))
                                                                                        (make-sets (map (lambda (exp) `(set ,(cadr exp) ,@(eliminate-nested-defines (cddr exp)))) x)))
                                                                                        `((applic (lambda-simple ,make-args (seq (,make-sets ,@(eliminate-nested-defines y)))) ,(make-list (length make-args) '(const #f)))))))))
            (else 
                        `(,(car exp) ,(get-lambda-args exp) ,@(eliminate-nested-defines (get-lambda-body exp))))))          
          ((and (list? exp) (eq? (car exp) 'lambda-opt))
           (cond ((inner-eliminate-nested-defines (get-lambda-opt-body exp) notNull-2?) 
                        `(,(car exp) ,(get-lambda-opt-args exp) ,(get-lambda-opt-rest-args exp) ,@(inner-eliminate-nested-defines (get-lambda-opt-body exp) 
                                                                                (lambda (x y)
                                                                                    (let* ((makeargs (map (lambda (exp) `(,@(cadadr exp))) x))
                                                                                        (makesets (map (lambda (exp) `(set ,(cadr exp) ,@(eliminate-nested-defines (cddr exp)))) x)))
                                                                                        `((applic (lambda-simple ,makeargs (seq (,makesets ,@(eliminate-nested-defines y)))) ,(make-list (length makeargs) '(const #f)))))))))
            (else 
                        `(,(car exp) ,(get-lambda-opt-args exp) ,(get-lambda-opt-rest-args exp) ,@(eliminate-nested-defines (get-lambda-opt-body exp))))))
          ((list? exp) (map eliminate-nested-defines exp))
(else exp))))
                        
          
(define remove-applic-lambda-nil
    (lambda (exp)
        (cond ((null? exp) exp)
              ((lambda-to-remove? exp) (remove-applic-lambda-nil (next-lambda exp)))
              ((list? exp) (map remove-applic-lambda-nil exp))
              (else exp))))         

(define lambda-to-remove?
    (lambda (exp)
        (and (list? exp) (equal? 'applic (car exp)) (equal? (caadr exp) 'lambda-simple) (null? (lambda-args exp)))))

(define next-lambda
    (lambda (exp)
        (car (cddadr exp))))

(define lambda-args
    (lambda (exp)
        (cadadr exp)))
        
        
;; Box-Set

(define get-lambda-args
    (lambda(x)
        (cadr x)))

(define get-lambda-body
    (lambda(x)
        (cddr x)))
        
(define get-lambda-opt-body
    (lambda(x)
        (cdddr x)))  
        
(define get-lambda-opt-args
    (lambda(x)
        (cadr x))) 
        
(define get-lambda-opt-rest-args
    (lambda(x)
        (caddr x))) 
        
       
        
;; (define set-box-var
;;   (lambda (lst)
;;     (map (lambda (p) `(set (var ,p) (box (var ,p)))) lst)))


(define box-set-args
  (lambda (arg lambda-b)
    (cond ((null? lambda-b) lambda-b)
          ((and (list? lambda-b) 
                (equal? 'set (car lambda-b)) 
                (equal? `(var ,arg) (get-lambda-args lambda-b))) 
            `(box-set (var ,arg) ,@(box-set-args arg (get-lambda-body lambda-b))))
          ((and (list? lambda-b) 
                (equal? `(var ,arg) lambda-b)) 
            `(box-get (var ,arg)))
          ((list? lambda-b) 
            `(,(box-set-args arg (car lambda-b)) ,@(box-set-args arg (cdr lambda-b))))
          (else lambda-b))))
          
(define make-box
  (lambda (lambda-body bounded)
    (cond   ((null? bounded) lambda-body)
            (else 
                (make-box (box-set-args (car bounded) lambda-body) (cdr bounded))))))
          
(define find-bounded-args
    (lambda (lambda-body lst)
      (cond ((null? lst) '())
            (((box? lambda-body) (car lst)) 
                (cons (car lst) (find-bounded-args lambda-body (cdr lst)))) 
            (else (find-bounded-args lambda-body (cdr lst))))))
            


(define box-set
  (lambda (exp)
    (cond ((null? exp) exp)
          
          ((and (list? exp) (ormap (lambda (x) (eq? (car exp) x)) '(lambda-simple lambda-var)))
            (let* ((lambda-body (get-lambda-body exp))
                   (bounded (if (eq? (car exp) 'lambda-simple)
                            (find-bounded-args lambda-body (get-lambda-args exp))
                            (find-bounded-args lambda-body (list (get-lambda-args exp))))))
             (if (null? bounded)
                 `(,(car exp) ,(get-lambda-args exp) ,@(box-set lambda-body))
                 (let* ((expr `(,@(map (lambda (x) `(set (var ,x) (box (var ,x)))) bounded) ,@(make-box lambda-body bounded))))
                 `(,(car exp) ,(get-lambda-args exp) (seq ,(splicing-begin expr)))))))
          
;;           ((and (list? e) (equal? 'lambda-simple (car e)))
;;            (let* ((body (cddr e))
;;                   (bound-params (filter (match? body) (cadr e))))
;;              (if (null? bound-params)
;;                  `(lambda-simple ,(cadr e) ,@(box-set body))
;;                  (let ((temp `(,@(set-box-var bound-params) ,@(box-set-helper body bound-params))))
;;                  `(lambda-simple ,(cadr e) (seq ,(remove-seq temp)))))))
                 
          ((and (list? exp) (equal? 'lambda-opt (car exp)))
           (let* ((lambda-body (get-lambda-opt-body exp))
                  (bounded (find-bounded-args lambda-body (append (get-lambda-opt-args exp) `(,(get-lambda-opt-rest-args exp))))))
             (if (null? bounded)
                 `(,(car exp) ,(get-lambda-opt-args exp) ,(get-lambda-opt-rest-args exp) ,@(box-set lambda-body))
                 (let* ((expr `(,@(map (lambda (x) `(set (var ,x) (box (var ,x)))) bounded) ,@(make-box lambda-body bounded))))
                 `(,(car exp) ,(get-lambda-opt-args exp) ,(get-lambda-opt-rest-args exp) (seq ,(splicing-begin expr)))))))
                 
;;           ((and (list? e) (equal? 'lambda-var (car e)))
;;            (let* ((body (cddr e))
;;                   (bound-params (filter (match? body) (list (cadr e)))))
;;              (if (null? bound-params)
;;                  `(lambda-var ,(cadr e) ,@(box-set body))
;;                  (let ((temp `(,@(set-box-var bound-params) ,@(box-set-helper body bound-params))))
;;                  `(lambda-var ,(cadr e) (seq ,(remove-seq temp)))))))
                 
          ((list? exp) (map box-set exp))
          
          (else exp))))

(define box?
  (lambda (lambda-body)
    (lambda (param)
      (and  (arg-bound? param lambda-body)
            (arg-get? param lambda-body)
            (arg-set? param lambda-body)))))
                              
(define arg-bound?
  (lambda (param lambda-body)
    (cond ((null? lambda-body) #f)
          ((and (list? lambda-body) 
                (ormap (lambda (x) (eq? (car lambda-body) x)) '(lambda-simple lambda-var)) 
                (if (eq? (car lambda-body) 'lambda-simple)
                    (not-in param (get-lambda-args lambda-body))
                    (not-in param (list (get-lambda-args lambda-body)))))
            (inner-bound? param (get-lambda-body lambda-body)))
          ((and (list? lambda-body) 
                (equal? 'lambda-opt (car lambda-body)) 
                (not-in param (append (get-lambda-opt-args lambda-body) `(,(get-lambda-opt-rest-args lambda-body))))) 
            (inner-bound? param (get-lambda-opt-body lambda-body)))
   ;       ((and (list? lambda-body) 
       ;         (equal? 'lambda-var (car lambda-body)) 
       ;         (not-in param (list (get-lambda-args lambda-body)))) 
      ;      (is-bound-in-lambda? param (cddr lambda-body)))
          ((list? lambda-body) 
            (or (arg-bound? param (car lambda-body)) (arg-bound? param (cdr lambda-body))))
          (else #f))))
          
(define not-in
    (lambda (arg lst)
        (not (member arg lst)))) 
        

(define inner-bound?
  (lambda (param lambda-body)
    (cond ((null? lambda-body) #f)
          ((and (list? lambda-body) 
                (ormap (lambda (x) (eq? (car lambda-body) x)) '(lambda-simple lambda-var)) 
                (if (eq? (car lambda-body) 'lambda-simple)
                    (not-in param (get-lambda-args lambda-body))
                    (not-in param (list (get-lambda-args lambda-body))))
             (inner-bound? param (get-lambda-body body))))
          ((and (list? lambda-body) 
                (equal? 'lambda-opt (car lambda-body)) 
                (not-in param (append (get-lambda-opt-args lambda-body) `(,(get-lambda-opt-rest-args lambda-body))))) 
            (inner-bound? param (get-lambda-opt-body lambda-body)))
    ;      ((and (list? body) 
     ;           (equal? 'lambda-var (car body)) 
     ;           (not (member param (list (get-lambda-args body))))) 
   ;          (inner-bound? param (get-lambda-body body)))
          ((equal? `(var ,param) lambda-body) #t)
          ((list? lambda-body) (or (inner-bound? param (car lambda-body)) (inner-bound? param (cdr lambda-body))))
          (else #f))))
          
(define arg-get?
  (lambda (param lambda-body)
    (cond ((null? lambda-body) #f)
          ((and (list? lambda-body) 
                (equal? `(var ,param) lambda-body)) #t)
          ((and (list? lambda-body) 
                (equal? 'set (car lambda-body)) 
                (equal? `(var ,param) (get-lambda-args lambda-body))) (arg-get? param (get-lambda-body lambda-body)))
          ((list? lambda-body) (or (arg-get? param (car lambda-body)) (arg-get? param (cdr lambda-body))))
          (else #f))))

(define arg-set?
  (lambda (param lambda-body)
    (cond ((null? lambda-body) #f)
          ((and (list? lambda-body) 
                (equal? 'set (car lambda-body)) 
                (equal? `(var ,param) (get-lambda-args lambda-body))) #t)
          ((list? lambda-body) (or (arg-set? param (car lambda-body)) (arg-set? param (cdr lambda-body))))
          (else #f))))
          
          
          
(define add-params
    (let ((list-y (lambda (x) (if (list? x) x (list x)))))
        (lambda (var-list env)
            (append (list (list-y var-list)) env))))
            
            
(define get-var-lex
    (letrec* (  (inner-get-var-lex (lambda (var env loc)
                    (if (null? env) #f
                            (let ((ans (search-var-lex var (car env) 0)))
                                (if ans (cons loc ans)
                                                    (inner-get-var-lex var (cdr env) (+ loc 1)))))))
                (search-var-lex (lambda (var lst loc)
                                (cond
                                    ((null? lst) #f)
                                    ((equal? (car lst) var) loc)
                                    (else (search-var-lex var (cdr lst) (+ loc 1)))  ))))
                           
    (lambda (var env)
        (let   ((var-ans (inner-get-var-lex var env 0)))
            (cond
                ((equal? var-ans #f) `(fvar ,var))
                ((= (car var-ans) 0) `(pvar ,var ,(cdr var-ans)))
                (else `(bvar ,var ,(- (car var-ans) 1) ,(cdr var-ans)))) ))))


(define pe->lex-pe
    (letrec ((inner-pe->lex-pe (lambda (pe env)
                            (cond
                                    ((not (pair? pe)) pe)
                                    ((or (equal? (car pe) 'lambda-simple) (equal? (car pe) 'lambda-var))
                                            (list (car pe) (cadr pe) (inner-pe->lex-pe (caddr pe) (add-params (cadr pe) env))))
                                    ((equal? (car pe) 'lambda-opt)
                                            (list (car pe) (cadr pe) (caddr pe) (inner-pe->lex-pe (cadddr pe) (add-params (append (cadr pe) (list (caddr pe))) env))))
                                    ((equal? (car pe) 'var) `,(get-var-lex (cadr pe) env))
                                    (else (map (lambda (newpe) (inner-pe->lex-pe newpe env)) pe))))))
        (lambda (pe)
            (inner-pe->lex-pe pe (list))
    )))

    
;; (define annotate-tc
;;     (letrec* (  (make-or (lambda (expr in-tp?)
;;                     (if (null? (cdr expr)) (list (helper (car expr) in-tp?))
;;                                     (append (list (helper (car expr) #f)) (make-or (cdr expr) in-tp?)))))
;;                 (make-if (lambda (expr in-tp?)
;;                             `(,(helper (car expr) #f) ,(helper (cadr expr) in-tp?) ,(helper (caddr expr) in-tp?))))
;;                 (lambda? (lambda (expr)
;;                             (or (equal? (car expr) 'lambda-simple) (equal? (car expr) 'lambda-var))))
;;                 (make-lambda-opt (lambda (expr in-tp?)
;;                             (let (  (title (car expr))
;;                                     (params (cadr expr))
;;                                     (opt (caddr expr))
;;                                     (body (cadddr expr)))
;;                             `(,title ,params ,opt ,(helper body #t)))))
;;                 (make-lambda (lambda (expr in-tp?)
;;                             (let (  (title (car expr))
;;                                     (params (cadr expr))
;;                                     (body (caddr expr)))
;;                             `(,title ,params ,(helper body #t)))))
;;                 (make-applic (lambda (expr in-tp?)
;;                             (let (  (op (car expr))
;;                                     (body (cadr expr)))
;;                             (if in-tp? `(tc-applic ,(helper op #f) ,(make-or body #f))
;;                                     `(applic ,(helper op #f) ,(make-or body #f))))))
;;                 (helper (lambda (expr in-tp?)
;;                                 (cond
;;                                     ((not (pair? expr)) expr)
;;                                     ((equal? (car expr) 'or) `(or ,(make-or (cdr expr) in-tp?)))
;;                                     ((equal? (car expr) 'if3) `(if3 ,(make-if (cdr expr) in-tp?)))
;;                                     ((equal? (car expr) 'seq) `(seq ,(make-or (cdr expr) in-tp?)))
;;                                     ((equal? (car expr) 'lambda-opt) (make-lambda-opt expr in-tp?))
;;                                     ((lambda? expr) (make-lambda expr in-tp?))
;;                                     ((equal? (car expr) 'applic) (make-applic (cdr expr) in-tp?))
;;                                     (else  expr))
;;                                                 )))
;;         (lambda (expr)
;;                 (helper expr #f)
;; )))

(define verify-list-and
    (lambda (lst cond1)
        (and (list? lst) cond1)))

(define annotate-tc 
    (letrec* (
        (make-if 
            (lambda (exp tp?)
                `(if3 ,(do-annotate (cadr exp) #f) ,(do-annotate (caddr exp) tp?) ,(do-annotate (cadddr exp) tp?))))
        (make-lambda-simple 
            (lambda (exp)
                `(lambda-simple ,(cadr exp) ,@(do-annotate (cddr exp) #t))))
        (make-lambda-opt
            (lambda (exp)
                `(lambda-opt ,(cadr exp) ,(caddr exp)  ,@(do-annotate (cdddr exp) #t))))
        (make-lambda-var
            (lambda (exp)
                 `(lambda-var ,(cadr exp) ,@(do-annotate (cddr exp) #t))))
        (make-seq-or
            (lambda (exp tp?)
                (let* 
                    (   (tail (list-tail exp (- (length exp) 1)))
                        (heads (reverse (cdr (reverse exp) ))))
                    (append (map annotate-tc heads) (do-annotate tail tp?)))))
        (do-annotate
            (lambda (exp tp?)
                (cond
                ((null? exp) exp)
                ((verify-list-and exp (or  (member (car exp) '(var fvar pvar bvar)) (equal? (car exp)                   'const))) exp)
                ((verify-list-and exp (member (car exp) '(seq or))) 
                    `(,(car exp) ,(make-seq-or (cadr exp) tp?)))
                ((verify-list-and exp (equal? (car exp) 'if3)) 
                    ;`(if3 ,(do-annotate (cadr exp) #f) ,(do-annotate (caddr exp) tp?) ,(do-annotate (cadddr exp) tp?)))
                    (make-if exp tp?))
                ((verify-list-and exp (member (car exp) '(def set box-set))) 
                    `(,(car exp) ,(cadr exp) ,@(do-annotate (cddr exp) #f)))
                ((and (list? exp) (equal? (car exp) 'box-get) `(,(car exp) ,(cadr exp) ,@(do-annotate (cddr exp) #f))))
                ((verify-list-and exp (equal? (car exp) 'lambda-simple)) 
                   ; `(lambda-simple ,(cadr exp) ,@(do-annotate (cddr exp) #t)))
                    (make-lambda-simple exp))
                ((verify-list-and exp (equal? (car exp) 'lambda-opt)) 
                    ;`(lambda-opt ,(cadr exp) ,(caddr exp)  ,@(do-annotate (cdddr exp) #t)))
                    (make-lambda-opt exp))
                ((verify-list-and exp (equal? (car exp) 'lambda-var)) 
                 ;   `(lambda-var ,(cadr exp) ,@(do-annotate (cddr exp) #t)))
                    (make-lambda-var exp))
                ((verify-list-and exp (equal? (car exp) 'applic))
                    (if tp? `(tc-applic ,@(do-annotate (cdr exp) #f)) `(applic ,@(do-annotate (cdr exp) #f))))
                ((list? exp) `(,(do-annotate (car exp) tp?) ,@(do-annotate (cdr exp) tp?)))
                (else exp)))))
    
    (lambda (expr)
        (do-annotate expr #f))
        ))
          
