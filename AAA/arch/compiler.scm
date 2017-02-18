;;; compiler.scm
;;; Programmers: Omri Gal & Carmel Levy, 2016-17

(case-sensitive #f)

(load "pc.scm")
(load "pattern-matcher.scm")
(load "qq.scm")
(load "lables.scm")

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
       (*delayed (lambda () <Sexpr>))
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
        (*delayed (lambda () <Sexpr>)) *star
        (*parser (char #\) ))
        (*caten 3)
        (*pack-with 
          (lambda (open exp close) `(,@exp))) 
    done))
    
       
(define <ImproperList>
    (new
        (*parser (char #\( ))
        (*delayed (lambda () <Sexpr>)) *plus
        (*parser (char #\.))
        (*delayed (lambda () <Sexpr>))
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
        (*delayed (lambda () <Sexpr>)) *star
        (*parser (char #\) ))
        (*caten 4)
        (*pack-with 
          (lambda (open atx exp close) `#(,@exp)))
    done))
    
(define <Quated>
    (new
        (*parser (char #\' ))
        (*delayed (lambda () <Sexpr>))
        (*caten 2)
        (*pack-with
            (lambda(qu exp)
                (list 'quote exp)))
    done))
    
(define <QuasiQuated>
    (new
        (*parser (char #\` ))
        (*delayed (lambda () <Sexpr>))
        (*caten 2)
        (*pack-with
            (lambda(qu exp)
                (list 'quasiquote exp)))
    done))
    
(define <Unquated>
    (new
        (*parser (char #\, ))
        (*delayed (lambda () <Sexpr>))
        (*caten 2)
        (*pack-with
            (lambda(qu exp)
                (list 'unquote exp)))
    done))
    
(define <UnquateAndSpliced>
    (new 
        (*parser (char #\, ))
        (*parser (char #\@ ))
        (*delayed (lambda () <Sexpr>))
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
        (*delayed (lambda () <Sexpr>))
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

(define <Sexpr>
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
                
                
;-------------------------------------------------------------------------
;                       Assignment 3
;-------------------------------------------------------------------------

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
                                                                                        `((applic (lambda-simple ,make-args (seq (,@make-sets ,@(eliminate-nested-defines y)))) ,(make-list (length make-args) '(const #f)))))))))
            (else 
                        `(,(car exp) ,(get-lambda-args exp) ,@(eliminate-nested-defines (get-lambda-body exp))))))          
          ((and (list? exp) (eq? (car exp) 'lambda-opt))
           (cond ((inner-eliminate-nested-defines (get-lambda-opt-body exp) notNull-2?) 
                        `(,(car exp) ,(get-lambda-opt-args exp) ,(get-lambda-opt-rest-args exp) ,@(inner-eliminate-nested-defines (get-lambda-opt-body exp) 
                                                                                (lambda (x y)
                                                                                    (let* ((makeargs (map (lambda (exp) `(,@(cadadr exp))) x))
                                                                                        (makesets (map (lambda (exp) `(set ,(cadr exp) ,@(eliminate-nested-defines (cddr exp)))) x)))
                                                                                        `((applic (lambda-simple ,makeargs (seq (,@makesets ,@(eliminate-nested-defines y)))) ,(make-list (length makeargs) '(const #f)))))))))
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
        
(define n->s
    (lambda (x)
        (number->string x)))
        
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
          
(define box?
  (lambda (lambda-body env params)
    (lambda (param)
      (and  
            (arg-set? param lambda-body)
            (arg-get? param lambda-body)
            (arg-bound? param params lambda-body env)))))
          
(define find-bounded-args
    (lambda (lambda-body lst)
      (cond ((null? lst) '())
            (((box? lambda-body '() lst) (car lst)) 
                (cons (car lst) (find-bounded-args lambda-body (cdr lst)))) 
            (else (find-bounded-args lambda-body (cdr lst))))))
            

(define change-to-box
    (lambda (params lambda-body)
       (if (null? params)
            (box-set lambda-body)
           (change-to-box (cdr params) (box-param (car params) lambda-body)))
       ))
       
(define box-param
    (lambda (param lambda-body)
            (cond
                ((null? lambda-body) lambda-body)
                ((and (list? lambda-body) (equal? (car lambda-body) 'lambda-simple) (member param (cadr lambda-body))) lambda-body)  
              ((and (list? lambda-body) (equal? (car lambda-body) 'lambda-opt)  (member param (append (cadr lambda-body) (list (caddr lambda-body))))) lambda-body)
              ((and (list? lambda-body) (equal? (car lambda-body) 'lambda-var)  (member param (list (cadr lambda-body)))) lambda-body)
                ((and (list? lambda-body) (equal? 'var (car lambda-body)) (equal? param (cadr lambda-body))) `(box-get (var ,(cadr lambda-body)) ,@(box-param param (cddr lambda-body))))
                ((and (list? lambda-body) (equal? 'set (car lambda-body)) (equal? param (cadadr lambda-body))) `(box-set (var ,(cadadr lambda-body)) ,@(box-param param (cddr lambda-body))))
                ((list? lambda-body) (map (lambda (x) (box-param param x)) lambda-body))
                (else lambda-body))
    ))


(define box-set
    (lambda (exp)
        (cond ((null? exp) exp)
              ((and (list? exp) (equal? (car exp) 'lambda-simple)) 
              (let ((boxing (filter (box? (get-lambda-body exp) '() (get-lambda-args exp)) (get-lambda-args exp))))
              
                  (if (null? boxing) `(lambda-simple ,(get-lambda-args exp) ,@(box-set (get-lambda-body exp)))   
                                  (let  ((do-box  (map (lambda(param) `(set (var ,param) (box (var ,param)))) boxing))
                                         (replace-body (splicing-begin (change-to-box boxing (get-lambda-body exp)))))
             `(lambda-simple (,@(get-lambda-args exp)) (seq  (,@do-box ,@replace-body)))))))
             
             ((and (list? exp) (equal? (car exp) 'lambda-opt)) 
              (let ((boxing (filter (box? (get-lambda-opt-body exp) '() (get-lambda-opt-args exp)) (append (get-lambda-opt-args exp) (list (get-lambda-opt-rest-args exp))))))
                  (if (null? boxing) `(lambda-opt ,(get-lambda-opt-args exp) ,(get-lambda-opt-rest-args exp) ,@(box-set (get-lambda-opt-body exp)))   
                                  (let  ((do-box  (map (lambda(param) `(set (var ,param) (box (var ,param)))) boxing))
                                         (replace-body (splicing-begin (change-to-box boxing (get-lambda-opt-body exp)))))
              `(lambda-opt (,@(get-lambda-opt-args exp)) ,(get-lambda-opt-rest-args exp) (seq  (,@do-box ,@replace-body)))))))
             
             ((and (list? exp) (equal? (car exp) 'lambda-var)) 
              (let ((boxing (filter (box? (get-lambda-body exp) '() (list (get-lambda-args exp))) (list (get-lambda-args exp)))))
                  (if (null? boxing) `(lambda-var ,(get-lambda-args exp) ,@(box-set (get-lambda-body exp)))   
                                  (let  ((do-box  (map (lambda(param) `(set (var ,param) (box (var ,param)))) boxing))
                                         (replace-body (splicing-begin (change-to-box boxing (get-lambda-body exp)))))
           `(lambda-var ,(get-lambda-args exp) (seq  (,@do-box ,@replace-body)))))))
             ((list? exp) (map box-set exp))
             (else exp)
             )
             ))   
             


(define arg-set?
  (lambda (param lambda-body)
    (cond ((null? lambda-body) #f)
           ((and (list? lambda-body) (equal? (car lambda-body) 'lambda-simple) (member param (get-lambda-args lambda-body))) #f)  
           ((and (list? lambda-body) (equal? (car lambda-body) 'lambda-opt)  (member param (append (get-lambda-args lambda-body) (list (caddr lambda-body))))) #f)
           ((and (list? lambda-body) (equal? (car lambda-body) 'lambda-var)  (member param (list (get-lambda-args lambda-body)))) #f)

          ((and (list? lambda-body) (equal? 'set (car lambda-body)) (equal? `(var ,param) (get-lambda-args lambda-body))) #t)
          ((list? lambda-body) (ormap (lambda (x) (arg-set? param x)) lambda-body))
          (else #f))))   



            
(define not-in
    (lambda (arg lst)
        (not (member arg lst)))) 
            
(define inner-bound
    (lambda (param env)
        (cond 
            ((null? env) #f)
            ((member param (car env)) #t)
            (else (inner-bound param (cdr env))))
            )) 
           
(define arg-bound?
    (lambda (param params lambda-body env)
        (cond
         ((null? lambda-body) #f)
         ((and (list? lambda-body) (equal? (car lambda-body) 'lambda-simple) (not-in param (get-lambda-args lambda-body))) (arg-bound? param (get-lambda-args lambda-body) (cddr lambda-body) (cons params env)))
         ((and (list? lambda-body) (equal? (car lambda-body) 'lambda-simple) (member param (get-lambda-args lambda-body))) #f)
        
         ((and (list? lambda-body) (equal? (car lambda-body) 'lambda-opt) (not-in param (append (get-lambda-args lambda-body) (caddr lambda-body)))) (arg-bound? param 
                        (append (get-lambda-opt-args lambda-body) (get-lambda-opt-rest-args lambda-body)) (cdddr lambda-body) (cons params env)))
         ((and (list? lambda-body) (equal? (car lambda-body) 'lambda-opt)  (member param (append (get-lambda-opt-args lambda-body) (list (get-lambda-opt-rest-args lambda-body))))) #f)
    
         ((and (list? lambda-body) (equal? (car lambda-body) 'lambda-var) (not-in param (list (get-lambda-args lambda-body)))) (arg-bound? param (get-lambda-args lambda-body) (get-lambda-lambda-body lambda-body) (cons params env)))
          ((and (list? lambda-body) (equal? (car lambda-body) 'lambda-var)  (member param (list (get-lambda-args lambda-body)))) #f)
         
         ((and (equal? `(var ,param) lambda-body) (inner-bound param env)) #t)
         ((list? lambda-body) (ormap (lambda (x) (arg-bound? param params x env)) lambda-body)) 
         
         (else #f))
        
        ))

          
(define arg-get?
  (lambda (param lambda-body)
        (cond ((null? lambda-body) #f)
              ((and (list? lambda-body) (equal? (car lambda-body) 'lambda-simple) (member param (get-lambda-args lambda-body))) #f)  
              ((and (list? lambda-body) (equal? (car lambda-body) 'lambda-opt)  (member param (append (get-lambda-args lambda-body) (list (caddr lambda-body))))) #f)
              ((and (list? lambda-body) (equal? (car lambda-body) 'lambda-var)  (member param (list (get-lambda-args lambda-body)))) #f)
              ((and (list? lambda-body) (equal? `(var ,param) lambda-body)) #t)
              ((and (list? lambda-body) (equal? 'set (car lambda-body)) (equal? `(var ,param) (get-lambda-args lambda-body))) (arg-get? param (get-lambda-body lambda-body)))
              ((list? lambda-body) (ormap (lambda (x) (arg-get? param x)) lambda-body))
             (else #f))         
             ))



          
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
                ((and (list? exp) (or  (member (car exp) '(var fvar pvar bvar)) (equal? (car exp) 'const))) exp)
                ((and (list? exp) (member (car exp) '(seq or))) 
                    `(,(car exp) ,(make-seq-or (cadr exp) tp?)))
                ((and (list? exp) (equal? (car exp) 'if3)) 
                    ;`(if3 ,(do-annotate (cadr exp) #f) ,(do-annotate (caddr exp) tp?) ,(do-annotate (cadddr exp) tp?)))
                    (make-if exp tp?))
                ((and (list? exp) (member (car exp) '(def set box-set))) 
                    `(,(car exp) ,(cadr exp) ,@(do-annotate (cddr exp) #f)))
                ((and (list? exp) (equal? (car exp) 'box-get) `(,(car exp) ,(cadr exp) ,@(do-annotate (cddr exp) #f))))
                ((and (list? exp) (equal? (car exp) 'lambda-simple)) 
                    (make-lambda-simple exp))
                ((and (list? exp) (equal? (car exp) 'lambda-opt)) 
                    (make-lambda-opt exp))
                ((and (list? exp) (equal? (car exp) 'lambda-var)) 
                    (make-lambda-var exp))
                ((and (list? exp) (equal? (car exp) 'applic))
                    (if tp? `(tc-applic ,@(do-annotate (cdr exp) #f)) `(applic ,@(do-annotate (cdr exp) #f))))
                ((list? exp) `(,(do-annotate (car exp) tp?) ,@(do-annotate (cdr exp) tp?)))
                (else exp)))))
    
    (lambda (expr)
        (do-annotate expr #f))
        ))

        
;-------------------------------------------------------------------------       
;-------------------------------------------------------------------------
;                       FINAL PROJECT
;-------------------------------------------------------------------------
;-------------------------------------------------------------------------

;-------------------------------------------------------------------------
;                       Code generator for functions
;-------------------------------------------------------------------------

(define code-gen
    (lambda (parsed-exp env) 
        (cond
            ((equal? (car parsed-exp) 'if3)             (CODE-GEN-if3 (cdr parsed-exp) env))
            ((equal? (car parsed-exp) 'const)           (CODE-GEN-const (cadr parsed-exp)))
            ((equal? (car parsed-exp) 'seq)             (CODE-GEN-seq-1 (cadr parsed-exp) env))
            ((equal? (car parsed-exp) 'or)              (CODE-GEN-or (cadr parsed-exp) env)) 
            ((equal? (car parsed-exp) 'applic)          (CODE-GEN-applic (cdr parsed-exp) env))
            ((equal? (car parsed-exp) 'lambda-simple)   (CODE-GEN-lambda-simple (cdr parsed-exp) env)) 
            ((equal? (car parsed-exp) 'pvar)            (CODE-GEN-pvar (caddr parsed-exp)))
            ((equal? (car parsed-exp) 'bvar)            (CODE-GEN-bvar (caddr parsed-exp) (cadddr parsed-exp)))
            ((equal? (car parsed-exp) 'fvar)            (CODE-GEN-fvar (cadr parsed-exp)))
            ((equal? (car parsed-exp) 'set)             (CODE-GEN-set  (cadr parsed-exp) (caddr parsed-exp) env))
            ((equal? (car parsed-exp) 'box)             (CODE-GEN-box  (cadr parsed-exp)))
            ((equal? (car parsed-exp) 'box-set)         (CODE-GEN-box-set (cadr parsed-exp) (caddr parsed-exp)  env))
            ((equal? (car parsed-exp) 'box-get)         (CODE-GEN-box-get (cadr parsed-exp)  env))
            ((equal? (car parsed-exp) 'lambda-opt)      (CODE-GEN-lambda-opt (cdr parsed-exp)  env)) 
;;            ((equal? (car parsed-exp) 'lambda-var) (code-gen-lambda-var (cdr parsed-exp)  env))
;;            ((equal? (car parsed-exp) 'def) (code-gen-def parsed-exp(cdr pe)  env))
            (else `(failed because of: ,@parsed-exp))
            )
    ))
    
    
(define CODE-GEN-if3
    (lambda (parsed-exp env)
            (let ((gen-test     (code-gen (car parsed-exp) env))
                  (gen-dit      (code-gen (cadr parsed-exp) env))
                  (gen-dif      (code-gen (caddr parsed-exp) env))
                  (L-else       (^label-if-else))
                  (L-exit       (^label-if-exit)))
            (string-append 
                NL
                "// ***CODE-GEN IF***"          NL
                gen-test                        NL 
                "  CMP(R0, IMM(SOB_FALSE));"    NL
                "  JUMP_EQ(" L-else ");"        NL
                gen-dit                         NL 
                "  JUMP(" L-exit ");"           NL
                L-else ":"                      NL 
                gen-dif                         NL 
                L-exit ":"                      NL
            ))
    )) 

    
(define CODE-GEN-const
    (lambda (parsed-exp)
        (string-append 
            "  MOV(R0, IMM("
            (n->s (lookup-const-table parsed-exp *const-table*))
            "));"
        )
    ))
    
    
(define CODE-GEN-seq-1
    (lambda (seq-exp env)
        (string-append "// ***CODE-GEN SEQ***" NL
                        (CODE-GEN-seq-2 seq-exp env))
    ))

    
(define CODE-GEN-seq-2
    (lambda (seq-exp env)
        (cond   ((null? seq-exp) NL)
                (else 
                    (string-append  (code-gen (car seq-exp) env) NL
                                    (CODE-GEN-seq-2 (cdr seq-exp) env))))))
                                    
(define CODE-GEN-or
    (lambda (or-exp env)
        (letrec*    ((L-exit             (^label-or-exit))
                     (CODE-GEN-or-exp    (lambda(or-exp env)
                                                    (cond   ((= (length or-exp) 1)
                                                                    (string-append
                                                                        (code-gen (car or-exp) env) NL
                                                                            L-exit ":" ))
                                                                            
                                                            (else (string-append
                                                                    (code-gen (car or-exp) env)     NL
                                                                    "  CMP(R0, IMM(SOB_FALSE));"    NL
                                                                    "  JUMP_NE(" L-exit ");"        NL
                                                                    (CODE-GEN-or-exp (cdr or-exp) env) NL))))))
                    (string-append 
                        NL
                        "// ***CODE-GEN OR***"                NL
                        (CODE-GEN-or-exp or-exp env)))))
                        

(define applic-add-null-if-needed-opt
    (lambda(args params)
        (if     (= (length args) (length params))
                (string-append "  PUSH(SOB_NIL);" NL))
                ""
        ))
     

(define applic-add-null-if-needed-var
    (lambda(params)
        (if     (= (length params) 0)
                (string-append "  PUSH(SOB_NIL);" NL))
                ""
        ))
                                

(define CODE-GEN-applic
    (lambda (applic-exp env)
        (letrec*    ((L-error-not-closure       (^label-applic-error-not-closure))
                     (func                      (car applic-exp))
                     (params                    (reverse (cadr applic-exp)))
                     (CODE-GEN-applic-params    (lambda(params env)     
                                                            (cond   ((null? params) "")
                                                                    (else (string-append
                                                                                (code-gen (car params) env)   NL
                                                                                "  PUSH(R0);"                 NL 
                                                                                (CODE-GEN-applic-params (cdr params) env)))
                                                            ))))
            (string-append
                 NL
                 "// ***CODE-GEN APPLIC***"                             NL
                 "// Push Parameters"                                   NL
                 (cond ((equal? (car func) 'lambda-opt) (applic-add-null-if-needed-opt (cadr func) params))
                       ((equal? (car func) 'lambda-var) (applic-add-null-if-needed-var params))
                       (else ""))
                (CODE-GEN-applic-params params env)
                "// Push Parameters Number"                              NL
                "  MOV(R0, IMM("(n->s (length params)) "));"   NL
                "  PUSH(R0);"                                            NL
                "// Push Closure"                                        NL
                (code-gen func env)                                      NL
                "  CMP(INDD(R0, 0), IMM(T_CLOSURE));"                    NL
                "  JUMP_NE(" L-error-not-closure ");"                    NL
                "// Push Environment"                                    NL
                "  PUSH(INDD(R0, 1));"                                   NL
                "// APPLIC"                                              NL
                "  CALLA(INDD(R0, 2));"                                  NL
                "  DROP(1);"                                             NL 
                "  POP(R1);"                                             NL
                "  DROP(R1);"                                            NL 
                ))
    ))
    
    
(define CODE-GEN-lambda-simple
    (lambda (lambda-exp env-depth)
        (let*   ((L-closure-body                (^label-closure-body))
                 (L-closure-error-arg-count     (^label-error-lambda-arg-count))
                 (L-closure-exit                (^label-closure-exit))
                 
                 (L-closure-loop-copy-env       (^label-closure-loop-copy-env))
                 (L-closure-loop-copy-env-end   (^label-closure-loop-copy-env-end))
                 (copy-env-loop                 (string-append
                                                    "// Copy environments"                          NL
                                                    "  MOV(R3, IMM(0));"                            NL 
                                                    "  MOV(R4, IMM(1));"                            NL 
                                                    L-closure-loop-copy-env ":"                     NL 
                                                    "  CMP(R3, IMM("(n->s env-depth)"));"           NL
                                                    "  JUMP_EQ (" L-closure-loop-copy-env-end ");"  NL 
                                                    "  MOV(INDD(R2, R4), INDD(R1, R3));"            NL
                                                    "  INCR(R3);"                                   NL
                                                    "  INCR(R4);"                                   NL 
                                                    "  JUMP("L-closure-loop-copy-env ");"           NL
                                                    L-closure-loop-copy-env-end ":"                 NL
                                                    ))
                
                (L-closure-loop-copy-stack      (^label-closure-loop-copy-stack))
                (L-closure-loop-copy-stack-end  (^label-closure-loop-copy-stack-end))
                (copy-stack-loop                (string-append
                                                    "// Copy from stack"                            NL
                                                    "  MOV(R4, IMM(0));"                            NL
                                                    "  MOV(R5, IMM(2));"                            NL
                                                    L-closure-loop-copy-stack ":"                   NL
                                                    "  CMP(R4, R3);"                                NL
                                                    "  JUMP_EQ(" L-closure-loop-copy-stack-end ");" NL 
                                                    "  MOV(INDD(INDD(R2,0),R4),FPARG(R5));"         NL
                                                    "  INCR(R4);"                                   NL 
                                                    "  INCR(R5);"                                   NL
                                                    "  JUMP(" L-closure-loop-copy-stack ");"        NL 
                                                    L-closure-loop-copy-stack-end ":"               NL  
                                                    )))
                 (string-append
                    NL
                    "// ***CODE-GEN LAMBDA-SIMPLE***"                   NL
                    "  MOV(R1, FPARG(0));"                              NL 
                    "  MOV(R3,IMM(" (n->s (+ 1 env-depth)) "));"        NL
                    "  PUSH(R3);"                                       NL
                    "  CALL(MALLOC);"                                   NL
                    "  DROP(1);"                                        NL
                    "  MOV(R2,R0);"                                     NL
                    copy-env-loop
                    "  CMP(IMM(" (n->s env-depth) "), IMM(0));"         NL 
                    "  JUMP_EQ(" L-closure-loop-copy-stack-end ")"      NL
                    "  MOV(R3,FPARG(1));"                               NL  
                    "  PUSH(R3);"                                       NL
                    "  CALL(MALLOC);"                                   NL
                    "  DROP(1);"                                        NL
                    "  MOV(INDD(R2,0), R0);"                            NL    
                    copy-stack-loop
                    "// Create Closure"                                 NL
                    "  PUSH(IMM(3));"                                   NL 
                    "  CALL(MALLOC);"                                   NL
                    "  DROP(1);"                                        NL 
                    "  MOV(INDD(R0,0),IMM(T_CLOSURE));"                 NL
                    "  MOV(INDD(R0,1),R2);"                             NL
                    "  MOV(INDD(R0,2),LABEL(" L-closure-body "));"      NL
                    "  JUMP(" L-closure-exit ");"                       NL NL
                    "// Closure body"                                   NL
                    L-closure-body ":"                                  NL
                    "  PUSH(FP);"                                       NL 
                    "  MOV(FP,SP);"                                     NL 
                    "  CMP(FPARG(1), IMM("(n->s (length (car lambda-exp)))"));" NL
                    "  JUMP_NE(" L-closure-error-arg-count ");"                 NL
                    (code-gen (cadr lambda-exp) (+ 1 env-depth))                NL
                    "  POP(FP);"                                                NL 
                    "  RETURN;"                                                 NL 
                    L-closure-exit ":"                                          NL
                    ))
            ))
      
      
(define CODE-GEN-pvar
    (lambda (minor)
            (string-append 
                "// CODE-GEN pvar"                          NL
                "  MOV(R0, FPARG(" (n->s (+ 2 minor)) "));" NL
                )
    ))
 
 
(define CODE-GEN-bvar
    (lambda (major minor)
            (string-append  
                "// CODE-GEN bvar"                           NL
                "  MOV(R1, FPARG(0));"                       NL
                "  MOV(R1, INDD(R1, " (n->s major)"));"      NL
                "  MOV(R1, INDD(R1, " (n->s minor)"));"      NL
                "  MOV(R0, R1);"                             NL
            )
    ))
    
    
(define CODE-GEN-fvar
    (lambda (fvar)
        (string-append 
                "// CODE-GEN fvar"                                                   NL
                "  MOV(R0, IND("(n->s (lookup-fvar-table fvar *fvar-table*)) "));"   NL
        )
    ))
    
    
(define CODE-GEN-set
    (lambda (var val env)
       (cond    
            ((equal? (car var) 'fvar) 
                        (CODE-GEN-set-fvar (cadr var) val env))
            
            ((equal? (car var) 'pvar) 
                        (CODE-GEN-set-pvar (caddr var) val env))
                        
            ((equal? (car var) 'bvar) 
                        (CODE-GEN-set-bvar (caddr var) (cadddr var) val env))
                        
            (else ""))
        ))

    
(define CODE-GEN-set-fvar
    (lambda (var val env)
         (string-append 
                "// CODE-GEN SET-fvar"                                                  NL
                (code-gen val env)                                                      NL 
                "  MOV(IND("(n->s (lookup-fvar-table var *fvar-table*)) "), R0);"       NL
                "  MOV(R0, IMM(SOB_VOID));"                                             NL
        )
        ))
    
    
(define CODE-GEN-set-pvar
    (lambda (minor val env)
            (string-append 
                "// CODE-GEN SET-pvar"                               NL
                (code-gen val env)                                   NL 
                "  MOV(FPARG(" (n->s (+ 2 minor)) "), R0);"          NL 
                "  MOV(R0, IMM(SOB_VOID));"                          NL
    )))
    

    
(define CODE-GEN-set-bvar
    (lambda (major minor val env)
                (string-append 
                    "// CODE-GEN SET-bvar"                      NL
                    (code-gen val env)                          NL 
                    "  MOV(R1, FPARG(0));"                      NL 
                    "  MOV(R1, INDD(R1, "(n->s major)"));"      NL
                    "  MOV(R1, INDD(R1, "(n->s minor)"));"      NL 
                    "  MOV(R1, R0);"                            NL 
                    "  MOV(R0, IMM(SOB_VOID));"                 NL
                )
    ))


    
(define CODE-GEN-box
    (lambda (pvar-exp)
            (let ((minor (caddr pvar-exp)))
            (string-append  
                "// CODE-GEN BOX"                               NL                
                "  MOV(R2, FPARG(" (n->s (+ minor 2)) "));"     NL 
                "  PUSH(IMM(1));"                               NL 
                "  CALL(MALLOC);"                               NL
                "  DROP(1);"                                    NL 
                "  MOV(IND(R0), R2);" 
            ))
    ))


(define CODE-GEN-box-get
    (lambda (var env)
       (cond               
            ((equal? (car var) 'pvar) 
                        (CODE-GEN-box-get-pvar (caddr var) env))
                        
            ((equal? (car var) 'bvar) 
                        (CODE-GEN-box-get-bvar (caddr var) (cadddr var) env))
                        
            (else ""))
        ))
    
    
(define CODE-GEN-box-get-pvar
    (lambda(minor env)
        (string-append 
                "// CODE-GEN BOX-GET-pvar"                NL
                "  MOV(R0, FPARG(" (n->s (+ minor 2))");" NL 
                "  MOV(R0, IND(R0));"                     NL
          )
    ))
    

(define CODE-GEN-box-get-bvar
    (lambda(major minor env)
        (string-append 
                "// CODE-GEN BOX-GET-bvar"                NL
                "  MOV(R0, FPARG(0));"                    NL
                "  MOV(R0, INDD(R0, " (n->s major) "));"  NL
                "  MOV(R0, INDD(R0, " (n->s minor) "));"  NL  
                "  MOV(R0, IND(R0));"                     NL
           )
    ))
    
    
(define CODE-GEN-box-set
    (lambda(var val env)    
       (cond               
            ((equal? (car var) 'pvar) 
                        (CODE-GEN-box-set-pvar (caddr var) val env))
                        
            ((equal? (car var) 'bvar) 
                        (CODE-GEN-box-set-bvar (caddr var) (cadddr var) val env))
                        
            (else ""))
        ))
        

(define CODE-GEN-box-set-pvar
    (lambda(minor val env)
        (string-append               
                "// CODE-GEN BOX-SET-pvar"                           NL
                (code-gen val env)                                   NL 
                "  MOV(IND(FPARG(" (n->s (+ 2 minor)) ")), R0);"     NL 
                "  MOV(R0, IMM(SOB_VOID));"                          NL
    )))
    

(define CODE-GEN-box-set-bvar 
    (lambda(major minor val env)
        (string-append
                "// CODE-GEN BOX-SET-bvar"                  NL
                (code-gen val env)                          NL 
                "  MOV(R1, FPARG(0));"                      NL 
                "  MOV(R1, INDD(R1, "(n->s major)"));"      NL
                "  MOV(R1, INDD(R1, "(n->s minor)"));"      NL 
                "  MOV(IND(R1), R0);"                       NL 
                "  MOV(R0, IMM(SOB_VOID));"                 NL
            )
        ))
        

(define CODE-GEN-lambda-opt
    (lambda (lambda-exp env-depth)
        (let*   ((L-opt-closure-body                (^label-closure-lambda-opt-body))
                 (L-opt-closure-exit                (^label-closure-lambda-opt-end))
                 (L-opt-error-arg-count             (^label-error-lambda-opt-arg-count))
                         
                 (L-closure-loop-copy-env       (^label-closure-lambda-opt-loop-copy-env))
                 (L-closure-loop-copy-env-end   (^label-closure-lambda-opt-loop-copy-env-end))
                 (copy-env-loop                 (string-append
                                                    "// Copy environments"                          NL
                                                    "  MOV(R3, IMM(0));"                            NL 
                                                    "  MOV(R4, IMM(1));"                            NL 
                                                    L-closure-loop-copy-env ":"                     NL 
                                                    "  CMP(R3, IMM("(n->s env-depth)"));"           NL
                                                    "  JUMP_EQ (" L-closure-loop-copy-env-end ");"  NL 
                                                    "  MOV(INDD(R2, R4), INDD(R1, R3));"            NL
                                                    "  INCR(R3);"                                   NL
                                                    "  INCR(R4);"                                   NL 
                                                    "  JUMP("L-closure-loop-copy-env ");"           NL
                                                    L-closure-loop-copy-env-end ":"                 NL
                                                    ))
                 
                 (L-closure-loop-copy-stack      (^label-closure-lambda-opt-loop-copy-stack))
                 (L-closure-loop-copy-stack-end  (^label-closure-lambda-opt-loop-copy-stack-end))
                 (copy-stack-loop                (string-append
                                                    "// Copy from stack"                            NL
                                                    "  MOV(R4, IMM(0));"                            NL
                                                    "  MOV(R5, IMM(2));"                            NL
                                                    L-closure-loop-copy-stack ":"                   NL
                                                    "  CMP(R4, R3);"                                NL
                                                    "  JUMP_EQ(" L-closure-loop-copy-stack-end ");" NL 
                                                    "  MOV(INDD(INDD(R2,0),R4),FPARG(R5));"         NL
                                                    "  INCR(R4);"                                   NL 
                                                    "  INCR(R5);"                                   NL
                                                    "  JUMP(" L-closure-loop-copy-stack ");"        NL 
                                                    L-closure-loop-copy-stack-end ":"               NL  
                                                    ))
                                                    
                  (L-closure-opt-loop-create-list           (^label-closure-lambda-opt-loop-create-list))
                  (L-closure-opt-loop-create-list-end       (^label-closure-lambda-opt-loop-create-list-end))
                  (change-opt-to-list             (string-append
                                                    "//Converting optional Parameters to List"          NL
                                                    "  MOV(R1, SOB_NIL);"                               NL
                                                    "  MOV(R4, FPARG(1));"                              NL
                                                    "  MOV(R5,"(n->s (length (car lambda-exp))) ");"    NL
                                                    "  INCR(R4);"                                       NL
                                                    "  INCR(R5);"                                       NL
                                              ;      "  MOV(R6, R4);"                                    NL 
                                              ;      "  MOV(R7, R5);"                                    NL 
                                              ;      "  SUB(R6, R7)"                                     NL 
                                                    L-closure-opt-loop-create-list ":"                  NL
                                                    "  CMP(R4, R5);"                                    NL 
                                                    "  JUMP_EQ(" L-closure-opt-loop-create-list-end ");" NL
                                                    "  PUSH(R1);"                                       NL 
                                                    "  PUSH(FPARG(R4));"                                NL 
                                                    "  CALL(MAKE_SOB_PAIR);"                            NL 
                                                    "  DROP(2);"                                        NL 
                                                    "  MOV(R1, R0);"                                    NL 
                                                    "  DECR(R4);"                                       NL
                                                    "  JUMP(" L-closure-opt-loop-create-list ");"       NL 
                                                    L-closure-opt-loop-create-list-end ":"              NL                                                                "  MOV(FPARG(" (n->s (+ (length (car lambda-exp)) 2)) "), R1);" nl 

                                                    )))
                 (string-append
                    NL
                    "// ***CODE-GEN LAMBDA-OPT***"                      NL
                    "  MOV(R1, FPARG(0));"                              NL 
                    "  MOV(R3,IMM(" (n->s (+ 1 env-depth)) "));"        NL
                    "  PUSH(R3);"                                       NL
                    "  CALL(MALLOC);"                                   NL
                    "  DROP(1);"                                        NL
                    "  MOV(R2, R0);"                                    NL
                    copy-env-loop
                    "  CMP(IMM(" (n->s env-depth) "), IMM(0));"         NL 
                    "  JUMP_EQ(" L-closure-loop-copy-stack-end ")"      NL
                    "  MOV(R3,FPARG(1));"                               NL  
                    "  PUSH(R3);"                                       NL
                    "  CALL(MALLOC);"                                   NL
                    "  DROP(1);"                                        NL
                    "  MOV(INDD(R2,0), R0);"                            NL    
                    copy-stack-loop
                    "// Create Closure"                                 NL
                    "  PUSH(IMM(3));"                                   NL 
                    "  CALL(MALLOC);"                                   NL
                    "  DROP(1);"                                        NL 
                    "  MOV(INDD(R0,0),IMM(T_CLOSURE));"                 NL
                    "  MOV(INDD(R0,1),R2);"                             NL
                    "  MOV(INDD(R0,2),LABEL(" L-opt-closure-body "));"  NL
                    "  JUMP(" L-opt-closure-exit ");"                   NL NL
                    "// Closure body"                                   NL
                    L-opt-closure-body ":"                              NL
                    "  PUSH(FP);"                                       NL 
                    "  MOV(FP,SP);"                                     NL 
                    "  CMP(FPARG(1), IMM("(n->s (length (car lambda-exp)))"));" NL
                    "  JUMP_LT(" L-opt-error-arg-count ");"                     NL
                    change-opt-to-list
                    (code-gen (caddr lambda-exp) (+ 1 env-depth))               NL
                    "  POP(FP);"                                                NL 
                    "  RETURN;"                                                 NL 
                    L-opt-closure-exit ":"                                      NL
                    )) 
            ))
                    
                                                    
                        

                                                
;-------------------------------------------------------------------------
;                       Const table
;-------------------------------------------------------------------------

(define NL (list->string (list #\newline)))

(define get-const
    (lambda (lst)
        (cadr lst)))
        
(define get-addr
    (lambda (lst)
        (car lst)))
        
(define *const-table* (list `(1     ,*void-object*  937610)
                            `(2     ,'()            722689)
                            `(3     #t              (741553 1))
                            `(5     #f              (741553 0)) 
                            `(7     2               (945311 2))
                            `(9     3               (945311 3)) 
                        ))

(define base-const-table (string-append 
                                    "// *** CONST TABLE ***"    NL
                                    "  ADD(IND(0), IMM(1000));" NL
                                    "  MOV(IND(1), T_VOID);"    NL
                                    "  MOV(IND(2), T_NIL);"     NL
                                    "  MOV(IND(3), T_BOOL);"    NL
                                    "  MOV(IND(4), IMM(1));"    NL
                                    "  MOV(IND(5), T_BOOL);"    NL
                                    "  MOV(IND(6), IMM(0));"    NL
                                    "  MOV(IND(7), T_INTEGER);" NL
                                    "  MOV(IND(8), IMM(2));"    NL
                                    "  MOV(IND(9), T_INTEGER);" NL
                                    "  MOV(IND(10), IMM(3));"   NL NL
                           ))
                                                               
(define lookup-const-table 
    (lambda (const const-table)
                (if (null? const-table) 
                    `(Constant ,const wasn't found in const table)
                    (if (eq? const (get-const (car const-table)))
                        (get-addr (car const-table))
                        (lookup-const-table const  (cdr const-table)))
                )))

                
                
;-------------------------------------------------------------------------
;                       Fvar table
;-------------------------------------------------------------------------   

(define get-fvar
    (lambda(lst)    
        (car lst)))
        
(define fet-fvar-addr
    (lambda(lst)
        (cadr lst)))
    

(define *fvar-table*  (list `(car   3001)
                            `(cdr   3002)
                            `(cons  3003)           
                            `(+     3004)
                            `(x     3005)
                        ))
                        
(define lookup-fvar-table
    (lambda (fvar fvar-table)
        (if (null? fvar-table) 
            `(fvar ,fvar wasn't found in fvar table)
             (if (eq? fvar (get-fvar (car fvar-table)))
                        (fet-fvar-addr (car fvar-table))
                        (lookup-fvar-table fvar  (cdr fvar-table))))
    ))
                                                            
;-------------------------------------------------------------------------
;                       File handling
;-------------------------------------------------------------------------

(define read_file
    (lambda (filename)
        (let ((input (open-input-file filename)))
            (letrec ((iter-chars
                        (lambda ()
                                (let ((c (read-char input)))
                                    (if (eof-object? c)
                                        (begin (close-input-port input) '())
                                        (cons c (iter-chars))))
                                            )))
                    (iter-chars)))
))

(define create_file
    (lambda(output code)
        (let ((file (open-output-file output)))
            (write_to_file PROLOGUE file)
            (write_to_file base-const-table file) 
            ;;TODO: previous line is only temperory here until we will have const table 
            (map (lambda (exp) (write_to_file exp file)) code)
            (write_to_file EPILOGUE file)
            (close-output-port file))
    ))
	
            
(define write_to_file
    (lambda(source target)
        (display source target)
     ;   (display "\n" target)))
        ))

;-------------------------------------------------------------------------
;                       Prepare for compilation
;-------------------------------------------------------------------------

(define scanner
    (lambda (raw_input)
        (let ((sexpr-list raw_input))
            (letrec ((scan-input 
                        (lambda (sexpr-list)
                            (<Sexpr> sexpr-list
                                (lambda (exp rest)
                                        (if (null? rest) (list exp)
                                            (cons exp (scan-input rest))))
                                (lambda (w) `(failed because of: ,@w))))))
                         (scan-input sexpr-list)))
    ))
 
(define semantic-analyzer
    (lambda (tokens)
        (map (lambda (sub-sexpr)
                              (pe->lex-pe 
                                (box-set 
                                    (remove-applic-lambda-nil
                                        (eliminate-nested-defines 
                                            (parse sub-sexpr))))))
            tokens)
    ))

(define iter-code-gen
    (lambda (ast)
         (map (lambda (ast-exp) (code-gen ast-exp 0)) ast)))
         
(define PROLOGUE (list->string (read_file "prologue.scm")))

(define EPILOGUE (list->string (read_file "epilogue.scm")))

;-------------------------------------------------------------------------
;                       Main
;-------------------------------------------------------------------------

(define compile-scheme-file
    (lambda (inputFile outputFile)
    
        (let* ( (stream      (read_file inputFile))
                (tokens      (scanner stream))
                (ast         (semantic-analyzer tokens))
        ;       (const-table (build-const-table ast))
        ;       (fvar-table  (build-fvar-table ast))
                (code-input  (iter-code-gen ast))) 
                
             (create_file outputFile code-input)

;              (create_file outputFile  (code-gen-primitive) code-input )
    )))
    
(define sofi
    (lambda()
        (compile-scheme-file "test.scm" "sofi.c")))
        
(define sa
    (lambda()
    (let* (     (stream      (read_file "test.scm"))
                (tokens      (scanner stream))
                (ast         (semantic-analyzer tokens)) )
                
                
                ast)))
