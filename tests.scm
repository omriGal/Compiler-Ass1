; Change to your own location
(load "~/Downloads/parser.so")
(load "~/Comp/compiler.scm")

(define <my-sexpr> <sexpr2>) ; Change to your sexpr name
(define <staff-sexpr> <sexpr>)

(define testVSstaff
	(lambda (input)
		(let ((my-res (test-string <my-sexpr> input))
		      (staff-res (test-string <staff-sexpr> input)))
			(display input)
			(display ": ")			
			(cond ((equal? my-res staff-res)
				(display "\033[1;32mSuccess!\033[0m") (newline) #t)
				(else (display "\033[1;31mFailed!\033[0m ") 
					(display ", expected: ")					
					(display staff-res)
					(display ", actual:")
					(display my-res)
					(newline)
					#f))
			)))
			
(define runTests
  (lambda (tests-name lst)
	(newline)
	(display tests-name)
	(display ":")
	(newline)
	(display "=============")
	(newline)
	(let ((results (map testVSstaff lst)))
	(cond ((andmap (lambda (exp) (equal? exp #t)) results)		
		(display "\033[1;32mSUCCESS!\033[0m\n") (newline) #t)
		(else (display "\033[1;31mFAILED!\033[0m\n") (newline) #f)))
))

(define runAllTests
  (lambda (lst)
    (let ((results (map (lambda (test) (runTests (car test) (cdr test))) lst)))
      	(cond ((andmap (lambda (exp) (equal? exp #t)) results)		
		(display "\033[1;32m !!!!! ALL TESTS SUCCEEDED !!!!\033[0m\n"))
		(else (display "\033[1;31m ##### SOME TESTS FAILED #####\033[0m\n")))
		(newline))
))		

(define booleanTests (list "#t" "#f" "   #t" "#f   " "  #t    "))
	 
(define numberTests
	(list "0" "9" "123" "0987" "-5" "+8" "   -456   "
	      "657" "0/4" "8/28" "-2/4" "+2/4" "-2 / 4"
	 ))
	  
(define charTests
	(list	"#\\a" "#\\B" "#\\9" "#\\space" "#\\lambda"
		"#\\newline" "#\\nul" "#\\page"
		"#\\return" "#\\tab" "#\\x41" "#\\x23" "#\\x20" "  #\\xab   "
		"#\\x0" "#\\x110000" "#\\abc" "#\\x"
	  ))
	  
(define stringTests
	(list
	  "\"\\\\\"" "\"\\t\"" "\"\\\"\"" "\"\\f\""	  	  
	  "\"\\n\"" "\"\\r\"" "\"\\x09af;\"" "\"\\x41;\""
	  "\" 4 1;\"" "    \"  Akuna Matata  \"    "
	  "\"\\x0;\""
	  "\"\\x110000;\"" 
	  "\"\\x40; ABC \\t \\\" \" "
	  ))

(define symbolTests
	(list 
	"0123456789" "abcdeABCDE" "!$^*-_=+<>?/" 
	"0123456789abcdeABCDE!$^*-_=+<>?/" 
	"    Hellomynameisasaf   "
	  ))
	  
	  
(define properListTests
	(list
	  "()" "(a)" "(\"abc\")" "(a #t #f -2/4 -14 0 \"\\t\" #\\lambda \"\\x41;\")"
	  "(#\\return)" "( (a b c) #t #f)" "(#\\a (a b .c ) -54/32)" "  ((a b.c))   "
	  ))	  
	  
(define improperListTests
	(list	   
	    "(.#t)" "(a b      #t c . (d e #f \"str1\"   )    )"
	    "(#t abc . a)"							
	    "(abc#t . a)"
	    "(#t #f #\\a abc . (a b c)  )"
	    "    (#\\a #\\b (\"a1234\" . 123) . #t)   "
	  ))
	  
(define vectorTests
  (list 
    "#()" "#(#\\lambda \"abc\" -56/38)" " #(1 2 3)   " " #(#\\a #\\b #t -015/54)  " 
  ))
  
(define quotedTests
  (list 
    "'123" "'#t" "'#\\lambda" "'\"123a\"" "  'a123   " 
  ))
  
(define quasiquotedTests
  (list 
    "`123" "`#t" "`#\\lambda" "`\"123a\"" "   `a123  " 
  ))
  
(define unquotedTests
  (list 
    ",123" ",#t" ",#\\lambda" ",\"123a\"" "   ,a123  " 
  ))
  
(define unquoteAndSplicedTests
  (list 
    ",@123" ",@#t" ",@#\\lambda" ",@\"123a\"" "  ,@a123   " 
  ))
  
(define infixArrayGetTests
	(list	  
	    "##1[2]"
	    "#% 1+2[3]"
	    "##-b[i][j][i+j]"
	    "##-123[+45/54]"
	    "##1[2+3]"
	    "##(1+2)[+30+40]"
	    "#%1+2[-50+60-70]"
	    "##1+2 [3+4+150]"
	    "##1+2   [   5+4]"
	    "##1*5[4*6]"  
	    "##5*4[+3*7*9*154]"
	    "  #%  5   /   2 [-4 + a *  -7/16  * 9 * 154 ] "
	    " ## 123a[ + bc321 ** 3  /  6]" 
	    "## a[0] + a[a[a[a[a[0]]]]]"
	    "##ABC[##a+2]"
	    "##ABC[###\\a]"
))  

(define infixFuncallTests
	(list	  
	    "##f()"
	    "##A(      )"
	    "##-FUNC()"
	    "##func(a,b,c,d,e,fgh)"
	    "## FunctionCall123 ( arg1  , arg2  , arg3 , arg5)"
	    "## func ( 1+2, 3*4)"
	    "#% -F1(F2(a,b,  F3(c)))"
)) 
	  
(define infixExpTests
	(list	
	    "## -5/4"
	    "#% -145"
	    "## 15"
	    "## Symbol123"
	    "#% -b"
	    "## - abc"
	    "## -abc"
	    "#% -(a ^ b ^ Sym1 * Sym2 + Sym3)"
	    "#% - 5+4"
	    "#% -5+40"
	    "## -(a+b)"
	    "## - a+b"
	    "## 5^-32/(28*12)"
	    "#% 5^4"
	    "## a ^ b ^ c"
	    "##5-4-3"
	    "#% i+j"
	    "##(1-2)"
	    "#% (1+2)*3 "
	    "##1   +    2"
	    "## (3+1)^(5--60/5)"
	    "##-123+45"
	    "##1+2+3"
	    "##1+2+30+40"
	    "#%1+2-50+60-70"
	    "##1+2"
	    "##1+2+3+4+150"
	    "##1+2   *    5+4"
	    "##1*5*4*6"  
	    "##((5*4)+(3*7)*9*154)"
	    " #% (1+2)*3 "
	    "  #%  5   /   (2 -4) + a *  -7/16  * 9 * 154  "
	    " ## 123a + bc321 ** 3  /  6" 
	    "##(b ^ 2 - 4 * a * c)"
	    "##(-b + d) / (2 * a)"
	    "## a ^ b ^ c ^ 3 ^ d + -50/45 ^ abc"
	    "## 8 ^ (7+8)[5][6]"
	    "## a + b * c * d"
	    "## a + b ^ c ^ d"
	    "#% a/b + c/d"
	    "##-a*-b"
	    "##-a-b-c"
	    "##a+b+c"	    
))

(define infixSexprEscapeTests 
  (list
    "## ## #t"
    "## #% \"abc\""
    "####a+b +c"
    "##-##a+b +c+d"
))

(define commentsTests
  (list
    "## 2 - 3 - 4"
    "## 2- #; 3 - 4 + 5 * 6 ^ 7 +8 #; 1+2^3"
    
    ; Line Comments
    " 2^5 ; ## 3+4+5"
    
    ; sexpr comment
    "## 2 #; \"abc\""
    "## #; 2+5 6+7"
    "## #; 2+5 -b+7"
    "## #; 2+5 abc+7"
    "#; #\\lambda \"Timon And Pumba\" #; (1 .2)"
    "## ( #; 1+2 1+3)"
    "( 1 . #; #\\a 2)"
    "( #\\r #; #\\a 2)"
    "`(#\\a #\\b) #; #\\c"
    "## #; 1+2 a-b[#;5+3 4+5 #; 1+5^7] #; #\\a"
    "#; \"345\" ## 2+ #; 3- 5*6 8 #; \"abc\""
    " \" Akuna Matata \" ; ABCE1234"
    "#; #\\lambda ## #; 5^64*-12/45 1+2+FUNC(a#;1+2+3,b,1+5) #; \"abcde\""   
   ))

(define MayerExamples
  (list
  
  "(cons
  #;this-is-in-infix
  #%f(x+y, x-z, x*t,
  #;this-is-in-prefix
  #%(g (cons x y)
  #;#%this-is-in-infix
  #%cons(x, y)
  #;#%this-is-in-infix
  #%list(x, y)
  #;#%this-is-in-infix
  #%h(#;this-is-in-prefix
  #%(* x y),
  #;this-is-in-prefix
  #%(expt x z))))
  #%2)"  
  
  "## 2 + #; 3 - 4 + 5 * 6 ^ 7 8"
  
  "(let* ((d ##sqrt(b ^ 2 - 4 * a * c))
  (x1 ##((-b + d) / (2 * a)))
  (x2 ##((-b - d) / (2 * a))))
  `((x1 ,x1) (x2 ,x2)))"
  
  "(let ((result ##a[0] + 2 * a[1] + 3 ^ a[2] - a[3] * b[i][j][i + j]))
  result)"

  "(let ((result (* n ##3/4^3 + 2/7^5)))
  (* result (f result) ##g(g(g(result, result), result), result)))"

  "## a[0] + a[a[0]] * a[a[a[0]]] ^ a[a[a[a[0]]]] ^ a[a[a[a[a[0]]]]]"

  "(define pi ##4 * atan(1))"

  "#%A[1]+A[2]*A[3]^B[4][5][6]"

  "`(the answer is ##2 * 3 + 4 * 5)"

  "(+ 1 ##2 + 3 a b c)"
  
  "##-a-b-c"

))

(runAllTests
  (list
      (cons "Boolean" booleanTests)
      (cons "Number" numberTests)
      (cons "Char" charTests)
      (cons "String" stringTests)
      (cons "Symbol" symbolTests)
      (cons "Vector" vectorTests)
      (cons "Quasiquoted" quasiquotedTests)
      (cons "Quoted" quotedTests)
      (cons "UnquoteAndSpliced" unquoteAndSplicedTests)
      (cons "Unquoted" unquotedTests)
      (cons "Proper List" properListTests)
      (cons "Improper List" improperListTests)
      (cons "InfixArrayGet" infixArrayGetTests)
      (cons "infixSexprEscape" infixSexprEscapeTests)
       (cons "InfixExp" infixExpTests)  
       (cons "InfixFuncall" infixFuncallTests)
      (cons "Comments" commentsTests)
      (cons "MayerExamples" MayerExamples)    
))

