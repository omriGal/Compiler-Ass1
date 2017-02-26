
(define NL (list->string (list #\newline)))

(define n->s
    (lambda (x)
        (number->string x)))

;-------------------------------------------------------------------------
;                       Fvar Codes
;-------------------------------------------------------------------------

(define get-fvar
    (lambda(lst)    
        (car lst)))
        
(define fet-fvar-addr
    (lambda(lst)
        (cadr lst)))
    

(define *base-fvar-table* `(     
                 ;           (append         0)
                            (apply          0)
                            (<              0)
                            (=              0)
                            (>              0)
                            (+              0)
                            (/              0)
                            (*              0)
                            (-              0)
                            (boolean?       0)
                            (car            0)
                            (cdr            0)
                            (char->integer  0)
                            (char?          0)
                            (cons           0)           
                            (denominator    0)
                    ;        (eq?               0)
                            (integer?       0)
                            (integer->char  0)
                    ;        (list              0)
                            (make-string    0)
                            (make-vector    0)
                     ;       (map               0)
                            (not            0)
                            (null?          0)
                            (number?        0)
                            (numerator      0)
                            (pair?          0)
                            (procedure?     0)
                            (rational?      0)
                            (remainder      0)
                            (set-car!       0)
                            (set-cdr!       0)
                            (string-length  0)
                            (string-ref     0)
                            (string-set!    0)
                     ;       (string->symbol    0)
                            (string?        0)
                     ;       (symbol?           0)
                     ;       (symbol->string    0)
                            (vector         0)
                            (vector-length  0)
                            (vector-ref     0)
                            (vector-set!    0)
                            (vector?        0)
                            (zero?          0)
                            
                        ))

(define *fvar-table* '())                           
          
(define fvar-addr 0)

          
(define generate-fvar-table 
  (lambda (ast addr)
    (set! *fvar-table* *base-fvar-table*)
    (set! fvar-addr addr)

    (give-fvar-addr *fvar-table*)
    (for-each add-fvar ast)
    fvar-addr
    ))
    
    
(define add-fvar
  (lambda (exp)
    (if (list? exp)
        (cond ((is-fvar? exp) (set-fvar! (cadr exp)))
              ((has-list? exp) (for-each add-fvar exp))))))

(define is-fvar?
  (lambda (exp)
    (and (not (null? exp))
         (equal? (car exp) 'fvar))))

(define set-fvar!
  (lambda (fvar)
    (if (not (member fvar (map car *fvar-table*)))
        
        (begin
            (set! *fvar-table* (append *fvar-table* `((,fvar ,fvar-addr))))
            (set! fvar-addr (+ fvar-addr 1 ))
            ))))


(define give-fvar-addr
    (lambda(fvar-table)
        (if (null? fvar-table) 
            fvar-addr
            (begin
                (set-car! (cdar fvar-table) fvar-addr)
                (set! fvar-addr (+ 1 fvar-addr))
                (give-fvar-addr (cdr fvar-table) )))))
                
(define lookup-fvar-table
    (lambda (fvar fvar-table)
        (if (null? fvar-table) 
            `(fvar ,fvar wasn't found in fvar table)
             (if (eq? fvar (get-fvar (car fvar-table)))
                        (fet-fvar-addr (car fvar-table))
                        (lookup-fvar-table fvar  (cdr fvar-table))))
    ))


(define MALLOC-CLOSURE
    (lambda(label) 
        (string-append
            "  PUSH(IMM(3));"                                   NL
            "  CALL(MALLOC);"                                   NL
            "  DROP(1);"                                        NL
            "  MOV(INDD(R0, 0), IMM(T_CLOSURE));"               NL
            "  MOV(INDD(R0, 1), IMM(0));"                       NL
            "  MOV(INDD(R0, 2), LABEL(" label "));"             NL
            )
        ))
    
(define FVAR-car
    (lambda ()
        (string-append 
            "// FVAR-car"                                       NL
            "  JUMP(L_car_closure);"                            NL 
            "L_car_code:"                                       NL 
            "  PUSH(FP);"                                       NL
            "  MOV(FP, SP);"                                    NL
            
            "  CMP(FPARG(1), IMM(1));"                          NL
            "  JUMP_NE(L_closure_error_args_count);"            NL
            "  MOV(R1, FPARG(2));"                              NL
            "  CMP(INDD(R1, 0), IMM(T_PAIR));"                  NL
            "  JUMP_NE(L_error_car_not_pair);"                  NL
            "  MOV(R0, INDD(R1, 1));"                           NL
            
            "  POP(FP);"                                        NL
            "  RETURN;"                                         NL NL

            "L_car_closure:"                                    NL
            (MALLOC-CLOSURE "L_car_code")
            "  MOV(IND(" (n->s (lookup-fvar-table 'car *fvar-table*)) "), R0);" NL NL
        )
    ))
    
    

(define FVAR-cdr
    (lambda ()
        (string-append 
            "// FVAR-cdr"                               NL
            "  JUMP(L_cdr_closure);"                    NL
            "L_cdr_code:"                               NL 
            "  PUSH(FP);"                               NL
            "  MOV(FP, SP);"                            NL
           
            "  CMP(FPARG(1), IMM(1));"                  NL
            "  JUMP_NE(L_closure_error_args_count);"    NL
            "  MOV(R1, FPARG(2));"                      NL
            "  CMP(INDD(R1, 0), IMM(T_PAIR));"          NL
            "  JUMP_NE(L_error_cdr_not_pair);"          NL
            "  MOV(R0, INDD(R1, 2));"                   NL
            
            "  POP(FP);"                                NL
            "  RETURN;"                                 NL NL
            
            "L_cdr_closure:"                            NL
            (MALLOC-CLOSURE "L_cdr_code")
            "  MOV(IND(" (n->s (lookup-fvar-table 'cdr *fvar-table*)) "), R0);" NL NL
         
        )
    ))


(define FVAR-cons
    (lambda ()
        (string-append
            "// FVAR-cons"                            NL
            "  JUMP(L_cons_closure);"                 NL
            "L_cons_code:"                            NL
            "  PUSH(FP);"                             NL
            "  MOV(FP, SP);"                          NL
            
            "  CMP(FPARG(1), IMM(2));"                NL
            "  JUMP_NE(L_closure_error_args_count);"  NL
            "  PUSH(FPARG(3));"                       NL
            "  PUSH(FPARG(2));"                       NL
            "  CALL(MAKE_SOB_PAIR);"                  NL
            "  DROP(2);"                              NL
            
            "  POP(FP);"                              NL
            "  RETURN;"                               NL NL
            
            "L_cons_closure:"                         NL
            (MALLOC-CLOSURE "L_cons_code")
            "  MOV(IND(" (n->s (lookup-fvar-table 'cons *fvar-table*)) "), R0);"  NL NL
        )
    ))
    
    
(define FVAR-boolean?
    (lambda ()
        (string-append
            "// FVAR-boolean?"                              NL
            "  JUMP(L_boolean_closure);"                    NL
            "L_boolean_code:"                               NL
            "  PUSH(FP);"                                   NL
            "  MOV(FP, SP);"                                NL
            
            "  CMP(FPARG(1), IMM(1));"                      NL
            "  JUMP_NE(L_closure_error_args_count);"        NL
            "  MOV(R1, FPARG(2));"                          NL
            "  CMP(INDD(R1, 0), IMM(T_BOOL));"              NL
            "  JUMP_EQ(L_boolean_true);"                    NL
            "  MOV(R0, IMM(SOB_FALSE));"                    NL
            "  JUMP(L_boolean_end);"                        NL
            "L_boolean_true:"                               NL
            "  MOV(R0, IMM(SOB_TRUE));"                     NL
            "L_boolean_end:"                                NL
            "  POP(FP);"                                    NL
            "  RETURN;"                                     NL NL
            
            "L_boolean_closure:"                            NL
            (MALLOC-CLOSURE "L_boolean_code")
            "  MOV(IND(" (n->s (lookup-fvar-table 'boolean? *fvar-table*)) "), R0);" NL NL
           
        )
    ))
    

(define FVAR-char?
    (lambda ()
        (string-append
            "// FVAR-char?"                              NL
            "  JUMP(L_char_closure);"                    NL
            "L_char_code:"                               NL
            "  PUSH(FP);"                                   NL
            "  MOV(FP, SP);"                                NL
            
            "  CMP(FPARG(1), IMM(1));"                      NL
            "  JUMP_NE(L_closure_error_args_count);"        NL
            "  MOV(R1, FPARG(2));"                          NL
            "  CMP(INDD(R1, 0), IMM(T_CHAR));"              NL
            "  JUMP_EQ(L_char_true);"                       NL
            "  MOV(R0, IMM(SOB_FALSE));"                    NL
            "  JUMP(L_char_end);"                        NL
            "L_char_true:"                                  NL
            "  MOV(R0, IMM(SOB_TRUE));"                     NL
            "L_char_end:"                                   NL
            "  POP(FP);"                                    NL
            "  RETURN;"                                     NL NL
            
            "L_char_closure:"                               NL
            (MALLOC-CLOSURE "L_char_code")
            "  MOV(IND(" (n->s (lookup-fvar-table 'char? *fvar-table*)) "), R0);" NL NL
           
        )
    ))
    

(define FVAR-integer?
    (lambda ()
        (string-append
            "// FVAR-integer?"                              NL
            "  JUMP(L_integer_closure);"                    NL
            "L_integer_code:"                               NL
            "  PUSH(FP);"                                   NL
            "  MOV(FP, SP);"                                NL
            
            "  CMP(FPARG(1), IMM(1));"                      NL
            "  JUMP_NE(L_closure_error_args_count);"        NL
            "  MOV(R1, FPARG(2));"                          NL
            "  CMP(INDD(R1, 0), IMM(T_INTEGER));"           NL
            "  JUMP_EQ(L_integer_true);"                    NL
            "  MOV(R0, IMM(SOB_FALSE));"                    NL
            "  JUMP(L_integer_end);"                        NL
            "L_integer_true:"                               NL
            "  MOV(R0, IMM(SOB_TRUE));"                     NL
            "L_integer_end:"                                NL
            "  POP(FP);"                                    NL
            "  RETURN;"                                     NL NL
            
            "L_integer_closure:"                            NL
            (MALLOC-CLOSURE "L_integer_code")
            "  MOV(IND(" (n->s (lookup-fvar-table 'integer? *fvar-table*)) "), R0);" NL NL
        )
    ))
    
    
(define FVAR-null?
    (lambda ()
        (string-append
            "// FVAR-null?"                              NL
            "  JUMP(L_null_closure);"                    NL
            "L_null_code:"                               NL
            "  PUSH(FP);"                                NL
            "  MOV(FP, SP);"                             NL
            
            "  CMP(FPARG(1), IMM(1));"                   NL
            "  JUMP_NE(L_closure_error_args_count);"     NL
            "  MOV(R1, FPARG(2));"                       NL
            "  CMP(INDD(R1, 0), IMM(T_NIL));"            NL
            "  JUMP_EQ(L_null_true);"                    NL
            "  MOV(R0, IMM(SOB_FALSE));"                 NL
            "  JUMP(L_null_end);"                        NL
            "L_null_true:"                               NL
            "  MOV(R0, IMM(SOB_TRUE));"                  NL
            "L_null_end:"                                NL
            "  POP(FP);"                                 NL
            "  RETURN;"                                  NL NL
            
            "L_null_closure:"                            NL
            (MALLOC-CLOSURE "L_null_code")
            "  MOV(IND(" (n->s (lookup-fvar-table 'null? *fvar-table*)) "), R0);" NL NL
        )
    ))

    
(define FVAR-pair?
    (lambda ()
        (string-append
            "// FVAR-pair?"                              NL
            "  JUMP(L_pair_closure);"                    NL
            "L_pair_code:"                               NL
            "  PUSH(FP);"                                NL
            "  MOV(FP, SP);"                             NL
            
            "  CMP(FPARG(1), IMM(1));"                   NL
            "  JUMP_NE(L_closure_error_args_count);"     NL
            "  MOV(R1, FPARG(2));"                       NL
            "  CMP(INDD(R1, 0), IMM(T_PAIR));"           NL
            "  JUMP_EQ(L_pair_true);"                    NL
            "  MOV(R0, IMM(SOB_FALSE));"                 NL
            "  JUMP(L_pair_end);"                        NL
            "L_pair_true:"                               NL
            "  MOV(R0, IMM(SOB_TRUE));"                  NL
            "L_pair_end:"                                NL
            "  POP(FP);"                                 NL
            "  RETURN;"                                  NL NL
            
            "L_pair_closure:"                            NL
            (MALLOC-CLOSURE "L_pair_code")
            "  MOV(IND(" (n->s (lookup-fvar-table 'pair? *fvar-table*)) "), R0);" NL NL
        )
    ))
    
    
(define FVAR-procedure?
    (lambda ()
        (string-append
            "// FVAR-procedure?"                            NL
            "  JUMP(L_procedure_closure);"                  NL
            "L_procedure_code:"                             NL
            "  PUSH(FP);"                                   NL
            "  MOV(FP, SP);"                                NL
            
            "  CMP(FPARG(1), IMM(1));"                      NL
            "  JUMP_NE(L_closure_error_args_count);"        NL
            "  MOV(R1, FPARG(2));"                          NL
            "  CMP(INDD(R1, 0), IMM(T_CLOSURE));"           NL
            "  JUMP_EQ(L_procedure_true);"                  NL
            "  MOV(R0, IMM(SOB_FALSE));"                    NL
            "  JUMP(L_procedure_end);"                      NL
            "L_procedure_true:"                             NL
            "  MOV(R0, IMM(SOB_TRUE));"                     NL
            "L_procedure_end:"                              NL
            "  POP(FP);"                                    NL
            "  RETURN;"                                     NL NL
            
            "L_procedure_closure:"                          NL
            (MALLOC-CLOSURE "L_procedure_code")
            "  MOV(IND(" (n->s (lookup-fvar-table 'procedure? *fvar-table*)) "), R0);" NL NL
        )
    ))

    
(define FVAR-string?
    (lambda ()
        (string-append
            "// FVAR-string?"                               NL
            "  JUMP(L_string_closure);"                     NL
            "L_string_code:"                                NL
            "  PUSH(FP);"                                   NL
            "  MOV(FP, SP);"                                NL
            
            "  CMP(FPARG(1), IMM(1));"                      NL
            "  JUMP_NE(L_closure_error_args_count);"        NL
            "  MOV(R1, FPARG(2));"                          NL
            "  CMP(INDD(R1, 0), IMM(T_STRING));"            NL
            "  JUMP_EQ(L_string_true);"                     NL
            "  MOV(R0, IMM(SOB_FALSE));"                    NL
            "  JUMP(L_string_end);"                         NL
            "L_string_true:"                                NL
            "  MOV(R0, IMM(SOB_TRUE));"                     NL
            "L_string_end:"                                 NL
            "  POP(FP);"                                    NL
            "  RETURN;"                                     NL NL
            
            "L_string_closure:"                             NL
            (MALLOC-CLOSURE "L_string_code")
            "  MOV(IND(" (n->s (lookup-fvar-table 'string? *fvar-table*)) "), R0);" NL NL
        )
    ))
    
    
(define FVAR-vector?
    (lambda ()
        (string-append
            "// FVAR-vector?"                               NL
            "  JUMP(L_vector_closure);"                     NL
            "L_vector_code:"                                NL
            "  PUSH(FP);"                                   NL
            "  MOV(FP, SP);"                                NL
            
            "  CMP(FPARG(1), IMM(1));"                      NL
            "  JUMP_NE(L_closure_error_args_count);"        NL
            "  MOV(R1, FPARG(2));"                          NL
            "  CMP(INDD(R1, 0), IMM(T_VECTOR));"            NL
            "  JUMP_EQ(L_vector_true);"                     NL
            "  MOV(R0, IMM(SOB_FALSE));"                    NL
            "  JUMP(L_vector_end);"                         NL
            "L_vector_true:"                                NL
            "  MOV(R0, IMM(SOB_TRUE));"                     NL
            "L_vector_end:"                                 NL
            "  POP(FP);"                                    NL
            "  RETURN;"                                     NL NL
            
            "L_vector_closure:"                             NL
            (MALLOC-CLOSURE "L_vector_code")
            "  MOV(IND(" (n->s (lookup-fvar-table 'vector? *fvar-table*)) "), R0);" NL NL
        )
    ))
    

(define FVAR-zero?
    (lambda ()
        (string-append
            "// FVAR-zero?"                                 NL
            "  JUMP(L_zero_closure);"                       NL
            "L_zero_code:"                                  NL
            "  PUSH(FP);"                                   NL
            "  MOV(FP, SP);"                                NL
            
            "  CMP(FPARG(1), IMM(1));"                      NL
            "  JUMP_NE(L_closure_error_args_count);"        NL
            "  MOV(R1, FPARG(2));"                          NL
            "  CMP(INDD(R1, 0), IMM(T_INTEGER));"           NL
            "  JUMP_NE(L_zero_code_false);"                 NL
            "  CMP(INDD(R1, 1), IMM(0));"                   NL
            "  JUMP_EQ(L_zero_code_true);"                  NL
            "L_zero_code_false:"                            NL
            "  MOV(R0, IMM(SOB_FALSE));"                    NL
            "  JUMP(L_zero_end);"                           NL
            "L_zero_code_true:"                             NL
            "  MOV(R0, IMM(SOB_TRUE));"                     NL
            "L_zero_end:"                                   NL
            "  POP(FP);"                                    NL
            "  RETURN;"                                     NL NL
            
            "L_zero_closure:"                               NL
            (MALLOC-CLOSURE "L_zero_code")
            "MOV(IND(" (n->s (lookup-fvar-table 'zero? *fvar-table*)) "), R0);" NL NL
        )
    )) 
    

    
(define FVAR-string-length
    (lambda ()
        (string-append
            "// FVAR-string-length"                                 NL
            "  JUMP(L_string_length_closure);"                      NL
            "L_string_length_code:"                                 NL 
            "  PUSH(FP);"                                           NL
            "  MOV(FP, SP);"                                        NL
            
            "  CMP(FPARG(1), IMM(1));"                              NL
            "  JUMP_NE(L_closure_error_args_count);"                NL
            "  MOV(R1, FPARG(2));"                                  NL
            "  CMP(INDD(R1, 0), IMM(T_STRING));"                    NL
            "  JUMP_NE(L_error_string_length_not_string);"          NL
           
            "  PUSH(INDD(R1, 1));"                                  NL
            "  CALL(MAKE_SOB_INTEGER);"                             NL
            "  DROP(1);"                                            NL
            
            "  POP(FP);"                                            NL
            "  RETURN;"                                             NL NL
            
            "L_string_length_closure:"                              NL
            (MALLOC-CLOSURE "L_string_length_code")
            "MOV(IND(" (n->s (lookup-fvar-table 'string-length *fvar-table*)) "), R0);" NL NL 
        )
    )) 
    
    
(define FVAR-vector-length
    (lambda ()
        (string-append
            "// FVAR-vector-length"                                 NL
            "  JUMP(L_vector_length_closure);"                      NL
            "L_vector_length_code:"                                 NL 
            "  PUSH(FP);"                                           NL
            "  MOV(FP, SP);"                                        NL
            
            "  CMP(FPARG(1), IMM(1));"                              NL
            "  JUMP_NE(L_closure_error_args_count);"                NL
            "  MOV(R1, FPARG(2));"                                  NL
            "  CMP(INDD(R1, 0), IMM(T_VECTOR));"                    NL
            "  JUMP_NE(L_error_vector_length_not_vector);"          NL
           
            "  PUSH(INDD(R1, 1));"                                  NL
            "  CALL(MAKE_SOB_INTEGER);"                             NL
            "  DROP(1);"                                            NL
            
            "  POP(FP);"                                            NL
            "  RETURN;"                                             NL NL
            
            "L_vector_length_closure:"                              NL
            (MALLOC-CLOSURE "L_vector_length_code")
            "MOV(IND(" (n->s (lookup-fvar-table 'vector-length *fvar-table*)) "), R0);" NL NL  
        )
    )) 
    
    

(define FVAR-string-ref
    (lambda ()
        (string-append
            "// FVAR-string-ref"                                NL
            "  JUMP(L_string_ref_closure);"                     NL
            "L_string_ref_code:"                                NL 
            "  PUSH(FP);"                                       NL
            "  MOV(FP, SP);"                                    NL
            
            "  CMP(FPARG(1), IMM(2));"                          NL
            "  JUMP_NE(L_closure_error_args_count);"            NL
            "  MOV(R1, FPARG(2));"                              NL
            "  CMP(INDD(R1, 0), IMM(T_STRING));"                NL
            "  JUMP_NE(L_error_string_ref_not_string);"         NL
            "  MOV(R2, FPARG(3));"                              NL
            "  CMP(INDD(R2, 0), IMM(T_INTEGER));"               NL
            "  JUMP_NE(L_error_string_ref_not_integer);"        NL
            
            "  MOV(R3,INDD(R2,1));"                             NL
            "  ADD(R3,IMM(2));"                                 NL
            
            "  PUSH(INDD(R1, R3));"                             NL
            "  CALL(MAKE_SOB_CHAR);"                            NL
            "  DROP(1);"                                        NL
                        
            "  POP(FP);"                                        NL
            "  RETURN;"                                         NL NL
            
            "L_string_ref_closure:"                             NL
            (MALLOC-CLOSURE "L_string_ref_code")
            "  MOV(IND(" (n->s (lookup-fvar-table 'string-ref *fvar-table*)) "), R0);" NL NL      
        )
    )) 
    
    
(define FVAR-vector-ref
    (lambda ()
        (string-append
            "// FVAR-vector-ref"                                NL
            "  JUMP(L_vector_ref_closure);"                     NL
            "L_vector_ref_code:"                                NL 
            "  PUSH(FP);"                                       NL
            "  MOV(FP, SP);"                                    NL
            
            "  CMP(FPARG(1), IMM(2));"                          NL
            "  JUMP_NE(L_closure_error_args_count);"            NL
            "  MOV(R1, FPARG(2));"                              NL
            "  CMP(INDD(R1, 0), IMM(T_VECTOR));"                NL
            "  JUMP_NE(L_error_vector_ref_not_vector);"         NL
            "  MOV(R2, FPARG(3));"                              NL
            "  CMP(INDD(R2, 0), IMM(T_INTEGER));"               NL
            "  JUMP_NE(L_error_vector_ref_not_integer);"        NL
            
            "  MOV(R3,INDD(R2,1));"                             NL
            "  ADD(R3,IMM(2));"                                 NL
            
            "  MOV(R0, INDD(R1,R3));"                           NL
                        
            "  POP(FP);"                                        NL
            "  RETURN;"                                         NL NL
            
            "L_vector_ref_closure:"                             NL
            (MALLOC-CLOSURE "L_vector_ref_code")
            "  MOV(IND(" (n->s (lookup-fvar-table 'vector-ref *fvar-table*)) "), R0);" NL NL      
        )
    ))
    
    
(define FVAR-make-vector
    (lambda ()
        (string-append
            "// FVAR make-vector"               NL
            "  JUMP(L_make_vector_closure);"    NL
            "L_make_vector_code:"               NL 
            "  PUSH(FP);"                       NL
            "  MOV(FP, SP);"                    NL
            
            "  CMP(FPARG(1), IMM(1));"          NL
            "  JUMP_NE(L_make_vector_2_args)"   NL 
           
            "  MOV(R1, FPARG(2));"              NL
            "  CMP(INDD(R1, 0), IMM(T_INTEGER));"   NL
            "  JUMP_NE(L_make_vector_not_integer);" NL
            
            "  PUSH(IMM(0));"                       NL
            "  CALL(MAKE_SOB_INTEGER);"             NL
            "  DROP(1);"                            NL
            "  MOV(R2, R0);"                        NL
            
            "  JUMP(L_make_vector_loop);"           NL
            
            "L_make_vector_2_args:"                 NL
            "  CMP(FPARG(1), IMM(2));"              NL
            "  JUMP_NE(L_closure_error_args_count);" NL
          
            "  MOV(R1, FPARG(2));"                  NL
            "  CMP(INDD(R1, 0), IMM(T_INTEGER));"   NL
            "  JUMP_NE(L_make_vector_not_integer);" NL
            
            "  MOV(R2, FPARG(3));"                  NL
            
            "L_make_vector_loop:"                   NL
            "  MOV(R3, INDD(R1,1));"                NL
            
            "L_make_vector_loop_1:"                 NL
            "  CMP(R3,IMM(0));"                     NL
            "  JUMP_EQ(L_make_vector_loop_end);"    NL
            "  PUSH(R2);"                           NL

            "  DECR(R3);"                           NL
            "  JUMP(L_make_vector_loop_1);"         NL
            
            "L_make_vector_loop_end:"
            "  PUSH(INDD(R1,1));"                   NL
            "  CALL(MAKE_SOB_VECTOR);"              NL
            "  DROP(INDD(R1,1));"                   NL
            "  DROP(IMM(1));"                       NL
           
            "  POP(FP);"                            NL
            "  RETURN;"                             NL NL
            
            "L_make_vector_closure:"                NL
            (MALLOC-CLOSURE "L_make_vector_code")
            "  MOV(IND(" (n->s (lookup-fvar-table 'make-vector *fvar-table*)) "), R0);" NL NL       
        )
    )) 
    
    
(define FVAR-<
    (lambda ()
        (string-append
            "// FVAR <"                         NL
            "  JUMP(L_lower_closure);"          NL
            "L_lower_code:"                     NL 
        
            "  PUSH(FP);"                       NL
            "  MOV(FP, SP);"                    NL
      
            "  MOV(R1,FPARG(1));"               NL
            "  CMP(R1, IMM(0));"                NL
            "  JUMP_EQ(L_closure_error_args_count);" NL
      
            "  MOV(R0,SOB_TRUE);"               NL  ;ans
            "  MOV(R2,IMM(0));"                 NL
      
            "  CMP(R1,IMM(1));"                 NL
            "  JUMP_EQ(L_lower_end);"           NL
             
            "  MOV(R3, FPARG(2));"                  NL
            "  CMP(INDD(R3,0), IMM(T_FRACTION));"   NL
            "  JUMP_EQ(L_lower_before_loop);"       NL
            
            "  PUSH(IMM(1));"                       NL
            "  PUSH(INDD(R3,1));"                   NL
            "  CALL(MAKE_SOB_FRACTION);"            NL
            "  DROP(IMM(2));"                       NL
            "  MOV(R3, R0);"                        NL
            
            "L_lower_before_loop:"                  NL
            "  MOV(R6, INDD(R3,1));"                NL
            "  MOV(R7, INDD(R3,2));"                NL

            "  INCR(R2);"                           NL
            
            "L_lower_loop:"                         NL
            "  CMP(R2, R1);"                        NL
            "  MOV(R0,SOB_TRUE);"                   NL  
            "  JUMP_EQ(L_lower_end);"               NL

            "  MOV(R3, FPARG(R2+2));"               NL
            "  CMP(INDD(R3,0), IMM(T_FRACTION));"   NL
            "  JUMP_EQ(L_lower_fraction);"          NL
            
            "  PUSH(IMM(1));"               NL
            "  PUSH(INDD(R3,1));"           NL
            "  CALL(MAKE_SOB_FRACTION);"    NL
            "  DROP(2);"                    NL
            "  MOV(R3, R0);"                NL
            
            "L_lower_fraction:"             NL
            "  MOV(R8, INDD(R3,1));"        NL
            "  MUL(R8, R7);"                NL
            "  MUL(R6, INDD(R3,2));"        NL
            
            "  CMP(R6,R8);"                 NL
            
            "  MOV(R6,R8);"                 NL
            "  MUL(R7, INDD(R3,2));"        NL
      
            "  INCR(R2);"                   NL
                        
            "  JUMP_LT(L_lower_loop);"      NL         
            
            "  MOV(R0,SOB_FALSE);"          NL   

            "L_lower_end:"                  NL
            "  POP(FP);"                    NL
            "  RETURN;"                     NL NL

            "L_lower_closure:"              NL
            (MALLOC-CLOSURE "L_lower_code")
            "  MOV(IND(" (n->s (lookup-fvar-table '< *fvar-table*)) "), R0);" NL NL       
        )
    )) 
    
              
(define FVAR-=
    (lambda ()
        (string-append
            "// FVAR ="                         NL
            "  JUMP(L_equal_closure);"          NL
            "L_equal_code:"                     NL 
            "  PUSH(FP);"                       NL
            "  MOV(FP, SP);"                    NL

            "  MOV(R1,FPARG(1));"               NL
            "  CMP(R1, IMM(0));"                NL
            "  JUMP_EQ(L_closure_error_args_count);" NL
      
            "  MOV(R0,SOB_TRUE);"               NL
            "  MOV(R2,IMM(0));"                 NL
      
            "  CMP(R1,IMM(1));"                 NL
            "  JUMP_EQ(L_equal_end);"           NL

            "  MOV(R3, FPARG(2));"              NL
            "  INCR(R2);"                       NL
            "  CMP(INDD(R3,0), IMM(T_INTEGER));" NL 
            
            "  JUMP_EQ(L_equal_int_loop);"      NL
            
            
            "L_equal_fraction_loop:"            NL
            "  CMP(R2, R1);"                    NL
            "  JUMP_EQ(L_equal_end);"           NL
      
            "  MOV(R4, FPARG(R2+2));"
            "  CMP(INDD(R4,0), IMM(T_FRACTION));"   NL
            "  JUMP_NE(L_equal_false);"             NL
            
            "  CMP(INDD(R3,1), INDD(R4,1));"        NL
            "  JUMP_NE(L_equal_false);"             NL

            "  CMP(INDD(R3,2), INDD(R4,2));"        NL
            "  JUMP_NE(L_equal_false);"             NL

            "  MOV(R3, R4);"                        NL
            "  INCR(R2);"                           NL
            "  JUMP_EQ(L_equal_fraction_loop);"     NL
            
            
            
            "L_equal_int_loop:"                 NL
            "  CMP(R2, R1);"                    NL
            "  JUMP_EQ(L_equal_end);"           NL
      
            "  MOV(R4, FPARG(R2+2));"
            "  CMP(INDD(R4,0), IMM(T_INTEGER));"    NL
            "  JUMP_NE(L_equal_false);"             NL
            "  CMP(INDD(R3,1), INDD(R4,1));"        NL
            "  MOV(R3, R4);"                        NL
            "  INCR(R2);"                           NL
            "  JUMP_EQ(L_equal_int_loop);"          NL
      
            "L_equal_false:"                        NL
            "  MOV(R0,SOB_FALSE);"                  NL

            "L_equal_end:"                          NL
            "  POP(FP);"                            NL
            "  RETURN; "                            NL NL
        
            "L_equal_closure:"             NL
            (MALLOC-CLOSURE "L_equal_code")
            "  MOV(IND(" (n->s (lookup-fvar-table '= *fvar-table*)) "), R0);" NL NL  
        )
    ))
    
    
(define FVAR->
    (lambda ()
        (string-append
            "// FVAR >"                      NL
            "  JUMP(L_greater_closure);"     NL
            "L_greater_code:"                NL 
            
            "  PUSH(FP);"                       NL
            "  MOV(FP, SP);"                    NL
      
            "  MOV(R1,FPARG(1));"               NL
            "  CMP(R1, IMM(0));"                NL
            "  JUMP_EQ(L_closure_error_args_count);" NL
      
            "  MOV(R0,SOB_TRUE);"               NL  ;ans
            "  MOV(R2,IMM(0));"                 NL
      
            "  CMP(R1,IMM(1));"                 NL
            "  JUMP_EQ(L_greater_end);"         NL
             
            "  MOV(R3, FPARG(2));"                  NL
            "  CMP(INDD(R3,0), IMM(T_FRACTION));"   NL
            "  JUMP_EQ(L_greater_before_loop);"     NL
            
            "  PUSH(IMM(1));"                       NL
            "  PUSH(INDD(R3,1));"                   NL
            "  CALL(MAKE_SOB_FRACTION);"            NL
            "  DROP(IMM(2));"                       NL
            "  MOV(R3, R0);"                        NL
            
            "L_greater_before_loop:"                NL
            "  MOV(R6, INDD(R3,1));"                NL
            "  MOV(R7, INDD(R3,2));"                NL

            "  INCR(R2);"                           NL
            
            "L_greater_loop:"                       NL
            "  CMP(R2, R1);"                        NL
            "  MOV(R0,SOB_TRUE);"                   NL  
            "  JUMP_EQ(L_greater_end);"             NL

            "  MOV(R3, FPARG(R2+2));"               NL
            "  CMP(INDD(R3,0), IMM(T_FRACTION));"   NL
            "  JUMP_EQ(L_greater_fraction);"        NL
            
            "  PUSH(IMM(1));"               NL
            "  PUSH(INDD(R3,1));"           NL
            "  CALL(MAKE_SOB_FRACTION);"    NL
            "  DROP(2);"                    NL
            "  MOV(R3, R0);"                NL
            
            "L_greater_fraction:"           NL
            "  MOV(R8, INDD(R3,1));"        NL
            "  MUL(R8, R7);"                NL
            "  MUL(R6, INDD(R3,2));"        NL
            
            "  CMP(R6,R8);"                 NL
            
            "  MOV(R6,R8);" 
            "  MUL(R7, INDD(R3,2));"        NL
      
            "  INCR(R2);"                   NL

            "  JUMP_GT(L_greater_loop);"    NL         
            
            "  MOV(R0,SOB_FALSE);"          NL   

            "L_greater_end:"                NL
            "  POP(FP);"                    NL
            "  RETURN;"                     NL NL

            "L_greater_closure:"             NL
            (MALLOC-CLOSURE "L_greater_code")
            "  MOV(IND(" (n->s (lookup-fvar-table '> *fvar-table*)) "), R0);" NL NL  
        )
    ))
    
    
   

(define FVAR-+
    (lambda ()
        (string-append
            "// FVAR +"                     NL
            "  JUMP(L_plus_closure);"       NL
            "L_plus_code:"                  NL 
            
            "  PUSH(FP);"                   NL
            "  MOV(FP, SP);"                NL

            "  MOV(R1, FPARG(1));"          NL 
            "  MOV(R2, IMM(0));"            NL
            "  MOV(R0, IMM(0));"            NL

            "  MOV(R6, IMM(0));"            NL  ; numerator ans
            "  MOV(R7, IMM(1));"            NL  ; denumerator ans
            
            "  CMP(R1, IMM(0));"            NL
            "  JUMP_EQ(L_plus_end);"        NL      

            "L_plus_loop:"                  NL
            "  CMP(R1, R2);"                NL
            "  JUMP_EQ(L_plus_end);"        NL
       
            "  MOV(R3, FPARG(R2+2));"       NL
            "  CMP(INDD(R3,0), IMM(T_FRACTION));"   NL
            "  JUMP_EQ(L_plus_fraction);"           NL
            
            "  PUSH(IMM(1));"                NL
            "  PUSH(INDD(R3,1));"           NL
            "  CALL(MAKE_SOB_FRACTION);"    NL
            "  DROP(2);"                    NL
            "  MOV(R3, R0);"                NL
            
            "L_plus_fraction:"              NL
            "  MOV(R8, INDD(R3,1));"        NL
            "  MUL(R8, R7);"                NL
            "  MUL(R6, INDD(R3,2));"        NL
            "  ADD(R6, R8);"                NL
            "  MUL(R7, INDD(R3,2));"        NL
                        
            "  INCR(R2); "                  NL
            "  JUMP(L_plus_loop);"          NL

            "L_plus_end:"                   NL
           
            "  PUSH(R7);"                   NL
            "  PUSH(R6);"                   NL
            "  CALL(GCD);"                  NL
            "  DROP(IMM(2));"               NL
            "  MOV(R9, R0);"                NL
            
            "  PUSH(R7);"                   NL
            "  PUSH(R6);"                   NL
            "  CALL(MAKE_SOB_FRACTION);"    NL
            "  DROP(IMM(2));"               NL

            "  PUSH(R9);"                   NL
            "  PUSH(R0);"                   NL
            "  CALL(FIX_FRACTION);"         NL      ; Returns T_INTEGER or T_FRACTION
            "  DROP(IMM(2));"               NL

            "  POP(FP);"                    NL
            "  RETURN; "                    NL NL

            "L_plus_closure:"               NL
            (MALLOC-CLOSURE "L_plus_code")
            "  MOV(IND(" (n->s (lookup-fvar-table '+ *fvar-table*)) "), R0);" NL NL  
        )
    ))
    
    
(define FVAR--
    (lambda ()
        (string-append
            "// FVAR -"                         NL
            "  JUMP(L_minus_closure);"          NL
            "L_minus_code:"                     NL 
            
            "  PUSH(FP);"                       NL
            "  MOV(FP, SP);"                    NL

            "  MOV(R1,FPARG(1));"               NL 
            "  MOV(R2,0);"                      NL
            "  MOV(R0,0);"                      NL

            "  CMP(R1,IMM(0));"                 NL
            "  JUMP_EQ(L_closure_error_args_count);" NL

            "  CMP(R1, IMM(1));"                NL
            "  JUMP_EQ(L_minus_change_sign);"   NL

            "  MOV(R3, FPARG(2));"                  NL
            "  CMP(INDD(R3,0), IMM(T_FRACTION));"   NL
            "  JUMP_EQ(L_minus_before_loop);"       NL
            
            "  PUSH(IMM(1));"                       NL
            "  PUSH(INDD(R3,1));"                   NL
            "  CALL(MAKE_SOB_FRACTION);"            NL
            "  DROP(IMM(2));"                       NL
            "  MOV(R3, R0);"                        NL
            
            "L_minus_before_loop:"                  NL
            "  MOV(R6, INDD(R3,1));"                NL
            "  MOV(R7, INDD(R3,2));"                NL

            "  INCR(R2);"                           NL

            "L_minus_loop:"                         NL
            "  CMP(R1,R2);"                         NL
            "  JUMP_EQ(L_minus_end);"               NL
           
            "  MOV(R3, FPARG(R2+2));"               NL
            "  CMP(INDD(R3,0), IMM(T_FRACTION));"   NL
            "  JUMP_EQ(L_minus_fraction);"          NL
            
            "  PUSH(IMM(1));"                       NL
            "  PUSH(INDD(R3,1));"                   NL
            "  CALL(MAKE_SOB_FRACTION);"            NL
            "  DROP(2);"                            NL
            "  MOV(R3, R0);"                        NL
            
            "L_minus_fraction:"                     NL
            "  MOV(R8, INDD(R3,1));"                NL
            "  MUL(R8, R7);"                        NL
            "  MUL(R6, INDD(R3,2));"                NL
            "  SUB(R6, R8);"                        NL
            "  MUL(R7, INDD(R3,2));"                NL           
                        
            "  INCR(R2);"                           NL
            "  JUMP(L_minus_loop);"                 NL

            
            "L_minus_change_sign:"                  NL 
            
            "  MOV(R3, FPARG(2));"                  NL
            "  CMP(INDD(R3,0),IMM(T_FRACTION));"            NL
            "  JUMP_EQ(L_minus_change_sign_fraction);"      NL
            
            "  PUSH(IMM(1));"                       NL
            "  PUSH(INDD(R3,1));"                   NL
            "  CALL(MAKE_SOB_FRACTION);"            NL
            "  DROP(IMM(2));"                       NL
            "  MOV(R3, R0);"                        NL

            
            "L_minus_change_sign_fraction:" NL
            "  MOV(R6, INDD(R3,1));"        NL
            "  MUL(R6, IMM(-1));"           NL
            "  MOV(R7, INDD(R3,2));"        NL

            
            "L_minus_end:"                  NL
            
            "  PUSH(R7);"                   NL
            "  PUSH(R6);"                   NL
            "  CALL(GCD);"                  NL
            "  DROP(IMM(2));"               NL
            "  MOV(R9, R0);"                NL
            
            "  PUSH(R7);"                   NL
            "  PUSH(R6);"                   NL
            "  CALL(MAKE_SOB_FRACTION);"    NL
            "  DROP(IMM(2));"               NL

            "  PUSH(R9);"                   NL
            "  PUSH(R0);"                   NL
            "  CALL(FIX_FRACTION);"         NL      ; Returns T_INTEGER or T_FRACTION
            "  DROP(IMM(2));"               NL
            
            "  POP(FP);"                    NL
            "  RETURN; "                    NL NL

            "L_minus_closure:"               NL
            (MALLOC-CLOSURE "L_minus_code")
            "  MOV(IND(" (n->s (lookup-fvar-table '- *fvar-table*)) "), R0);" NL NL  
        )
    ))
    
    
(define FVAR-*
    (lambda ()
        (string-append
            "// FVAR *"                    NL
            "  JUMP(L_mul_closure);"       NL
            "L_mul_code:"                  NL 
            
            "  PUSH(FP);"                   NL
            "  MOV(FP, SP);"                NL

            "  MOV(R1,FPARG(1));"           NL 
            "  MOV(R2,0);"                  NL
            "  MOV(R0,1);"                  NL

            "  MOV(R6, IMM(1));"            NL  ; numerator ans
            "  MOV(R7, IMM(1));"            NL  ; denumerator ans
            
            "  CMP(R1,IMM(0));"             NL
            "  JUMP_EQ(L_mul_end);"         NL      

            "L_mul_loop:"                   NL
            "  CMP(R1,R2);"                 NL
            "  JUMP_EQ(L_mul_end);"         NL

            "  MOV(R3, FPARG(R2+2));"               NL
            "  CMP(INDD(R3,0), IMM(T_FRACTION));"   NL
            "  JUMP_EQ(L_mul_fraction);"            NL
            
            "  PUSH(IMM(1));"                       NL
            "  PUSH(INDD(R3,1));"                   NL
            "  CALL(MAKE_SOB_FRACTION);"            NL
            "  DROP(2);"                            NL
            "  MOV(R3, R0);"                        NL
            
            "L_mul_fraction:"                       NL
            "  MUL(R6, INDD(R3,1));"                NL
            "  MUL(R7, INDD(R3,2));"                NL
    
            "  INCR(R2); "                  NL
            "  JUMP(L_mul_loop);"           NL

            "L_mul_end:"                    NL
            "  PUSH(R7);"                   NL
            "  PUSH(R6);"                   NL
            "  CALL(GCD);"                  NL
            "  DROP(IMM(2));"               NL
            "  MOV(R9, R0);"                NL
            
            "  PUSH(R7);"                   NL
            "  PUSH(R6);"                   NL
            "  CALL(MAKE_SOB_FRACTION);"    NL
            "  DROP(IMM(2));"               NL

            "  PUSH(R9);"                   NL
            "  PUSH(R0);"                   NL
            "  CALL(FIX_FRACTION);"         NL      ; Returns T_INTEGER or T_FRACTION
            "  DROP(IMM(2));"               NL

            "  POP(FP);"                    NL
            "  RETURN; "                    NL NL

            "L_mul_closure:"               NL
            (MALLOC-CLOSURE "L_mul_code")
            "  MOV(IND(" (n->s (lookup-fvar-table '* *fvar-table*)) "), R0);" NL NL  
        )
    ))
    
    
(define FVAR-/
    (lambda ()
        (string-append
            "// FVAR /"                         NL
            "  JUMP(L_div_closure);"            NL
            "L_div_code:"                       NL 
            
            "  PUSH(FP);"                       NL
            "  MOV(FP, SP);"                    NL
            
            "  MOV(R1,FPARG(1));"               NL 
            "  MOV(R2,0);"                      NL
            "  MOV(R0,0);"                      NL

            "  CMP(R1,IMM(0));"                 NL
            "  JUMP_EQ(L_closure_error_args_count);" NL

            "  CMP(R1,IMM(1));"                 NL
            "  JUMP_EQ(L_div_1_arg);"           NL

            "  MOV(R3, FPARG(2));"                  NL
            "  CMP(INDD(R3,0), IMM(T_FRACTION));"   NL
            "  JUMP_EQ(L_div_before_loop);"         NL
            
            "  PUSH(IMM(1));"                       NL
            "  PUSH(INDD(R3,1));"                   NL
            "  CALL(MAKE_SOB_FRACTION);"            NL
            "  DROP(IMM(2));"                       NL
            "  MOV(R3, R0);"                        NL
            
            "L_div_before_loop:"                    NL
            "  MOV(R6, INDD(R3,1));"                NL
            "  MOV(R7, INDD(R3,2));"                NL
            
            "  INCR(R2);"                           NL          
            
            ;*******************************************
            
            "L_div_loop:"                           NL
            "  CMP(R1,R2);"                         NL
            "  JUMP_EQ(L_div_end);"                 NL
            
            "  MOV(R3, FPARG(R2+2));"               NL
            "  CMP(INDD(R3,0), IMM(T_FRACTION));"   NL
            "  JUMP_EQ(L_div_fraction);"            NL
            
            "  PUSH(IMM(1));"                       NL
            "  PUSH(INDD(R3,1));"                   NL
            "  CALL(MAKE_SOB_FRACTION);"            NL
            "  DROP(2);"                            NL
            "  MOV(R3, R0);"                        NL
            
            "L_div_fraction:"                       NL

            "  CMP(IMM(0),INDD(R3,1));"             NL
            "  JUMP_EQ(L_division_by_zero);"        NL
            
            "  MUL(R6,INDD(R3,2));"                 NL
            "  MUL(R7,INDD(R3,1));"                 NL

            "  PUSH(R7);"                           NL
            "  PUSH(R6);"                           NL
            "  CALL(MAKE_SOB_FRACTION);"            NL
            "  DROP(IMM(2));"                       NL

            
            "  MOV(R3, R0);"                        NL
            
            "  PUSH(R1);"                           NL
            "  PUSH(R2);"                           NL

            "  PUSH(R3);"                           NL
            "  CALL(FIX_DIV_FRACTION);"             NL
            "  DROP(IMM(1));"                       NL
            
            "  POP(R2);"                            NL
            "  POP(R1);"                            NL

            "  MOV(R6, INDD(R0,1));"                NL
            "  MOV(R7, INDD(R0,2));"                NL

            "  INCR(R2);"                           NL
            "  JUMP(L_div_loop);"                   NL

            "L_div_1_arg:"                          NL 
            
            "  MOV(R3, FPARG(2));"                  NL
            "  CMP(INDD(R3,0),IMM(T_FRACTION));"    NL
            "  JUMP_EQ(L_div_1_arg_fraction);"      NL
            
            "  PUSH(IMM(1));"                       NL
            "  PUSH(INDD(R3,1));"                   NL
            "  CALL(MAKE_SOB_FRACTION);"            NL
            "  DROP(IMM(2));"                       NL
            "  MOV(R3, R0);"                        NL

            
            "L_div_1_arg_fraction:"                 NL
            "  PUSH(INDD(R3,1));"                   NL
            "  PUSH(INDD(R3,2));"                   NL
            "  CALL(MAKE_SOB_FRACTION);"            NL
            "  DROP(IMM(2));"                       NL
            "  MOV(R3, R0);"                        NL
            
            "  PUSH(R3);"                           NL
            "  CALL(FIX_DIV_FRACTION);"             NL
            "  DROP(IMM(1));"                       NL
            
            "  MOV(R6, INDD(R0,1));"                NL
            "  MOV(R7, INDD(R0,2));"                NL
           
            
            "L_div_end:"                    NL
            "  PUSH(R7);"                   NL
            "  PUSH(R6);"                   NL
            "  CALL(GCD);"                  NL
            "  DROP(IMM(2));"               NL
            "  MOV(R9, R0);"                NL
            
            "  PUSH(R7);"                   NL
            "  PUSH(R6);"                   NL
            "  CALL(MAKE_SOB_FRACTION);"    NL
            "  DROP(IMM(2));"               NL

            "  PUSH(R9);"                   NL
            "  PUSH(R0);"                   NL
            "  CALL(FIX_FRACTION);"         NL      ; Returns T_INTEGER or T_FRACTION
            "  DROP(IMM(2));"               NL
            
            "  POP(FP);"                    NL
            "  RETURN; "                    NL NL

            "L_div_closure:"               NL
            (MALLOC-CLOSURE "L_div_code")
            "  MOV(IND(" (n->s (lookup-fvar-table '/ *fvar-table*)) "), R0);" NL NL  
        )
    ))
    
    
    
(define FVAR-char->integer
    (lambda ()
        (string-append
            "// FVAR-char->integer"             NL
            "  JUMP(L_char2int_closure);"       NL
            "L_char2int_code:"                  NL
            "  PUSH(FP);"                       NL
            "  MOV(FP, SP);"                    NL
            
            "  MOV(R1,FPARG(1));"               NL 
            "  CMP(R1,IMM(1));"                 NL
            "  JUMP_NE(L_closure_error_args_count);" NL
            
            "  MOV(R0, FPARG(2));"              NL
            "  CMP(INDD(R0, 0), IMM(T_CHAR));"  NL
            "  JUMP_NE(L_char2int_not_char);"  NL

            
            "  MOV(R0, INDD(R0,1));"            NL
            "  PUSH(R0);"                       NL
            "  CALL(MAKE_SOB_INTEGER);"         NL
            "  DROP(IMM(1));"                   NL
            
            "  POP(FP);"                        NL
            "  RETURN;"                         NL NL
            
            "L_char2int_closure:"               NL
            (MALLOC-CLOSURE "L_char2int_code")
            "  MOV(IND(" (n->s (lookup-fvar-table 'char->integer *fvar-table*)) "), R0);" NL NL
        )
    ))
    
(define FVAR-char->integer
    (lambda ()
        (string-append
            "// FVAR char->integer"             NL
            "  JUMP(L_char2int_closure);"       NL
            "L_char2int_code:"                  NL
            "  PUSH(FP);"                       NL
            "  MOV(FP, SP);"                    NL
            
            "  MOV(R1,FPARG(1));"               NL 
            "  CMP(R1,IMM(1));"                 NL
            "  JUMP_NE(L_closure_error_args_count);" NL
            
            "  MOV(R0, FPARG(2));"              NL
            "  CMP(INDD(R0, 0), IMM(T_CHAR));"  NL
            "  JUMP_NE(L_char2int_not_char);"  NL

            
            "  MOV(R0, INDD(R0,1));"            NL
            "  PUSH(R0);"                       NL
            "  CALL(MAKE_SOB_INTEGER);"         NL
            "  DROP(IMM(1));"                   NL
            
            "  POP(FP);"                        NL
            "  RETURN;"                         NL NL
            
            "L_char2int_closure:"               NL
            (MALLOC-CLOSURE "L_char2int_code")
            "  MOV(IND(" (n->s (lookup-fvar-table 'char->integer *fvar-table*)) "), R0);" NL NL
        )
    ))
    
    
(define FVAR-integer->char
    (lambda ()
        (string-append
            "// FVAR integer->char"             NL
            "  JUMP(L_int2char_closure);"       NL
            "L_int2char_code:"                  NL
            "  PUSH(FP);"                       NL
            "  MOV(FP, SP);"                    NL
            
            "  MOV(R1,FPARG(1));"               NL 
            "  CMP(R1,IMM(1));"                 NL
            "  JUMP_NE(L_closure_error_args_count);" NL
            
            "  MOV(R0, FPARG(2));"              NL
            "  CMP(INDD(R0, 0), IMM(T_INTEGER));"  NL
            "  JUMP_NE(L_int2char_not_int);"    NL

            
            "  MOV(R0, INDD(R0,1));"            NL
            "  PUSH(R0);"                       NL
            "  CALL(MAKE_SOB_CHAR);"            NL
            "  DROP(IMM(1));"                   NL
            
            "  POP(FP);"                        NL
            "  RETURN;"                         NL NL
            
            "L_int2char_closure:"               NL
            (MALLOC-CLOSURE "L_int2char_code")
            "  MOV(IND(" (n->s (lookup-fvar-table 'integer->char *fvar-table*)) "), R0);" NL NL
        )
    ))
    
    
(define FVAR-set-car!
    (lambda ()
        (string-append 
            "// FVAR set-car!"                                  NL
            "  JUMP(L_set_car_closure);"                        NL 
            
            "L_set_car_code:"                                   NL 
            "  PUSH(FP);"                                       NL
            "  MOV(FP, SP);"                                    NL
            
            "  CMP(FPARG(1), IMM(2));"                          NL
            "  JUMP_NE(L_closure_error_args_count);"            NL
            
            "  MOV(R0, FPARG(2));"                              NL
            "  CMP(INDD(R0, 0), IMM(T_PAIR));"                  NL
            "  JUMP_NE(L_error_set_car_not_pair);"              NL
            
            "  MOV(R1, FPARG(3));"                              NL
            "  MOV(INDD(R0,1),R1);"                             NL
            
            "  MOV(R0, SOB_VOID);"                              NL           

            "  POP(FP);"                                        NL
            "  RETURN;"                                         NL NL

            "L_set_car_closure:"                                NL
            (MALLOC-CLOSURE "L_set_car_code")
            "  MOV(IND(" (n->s (lookup-fvar-table 'set-car! *fvar-table*)) "), R0);" NL NL
        )
    ))
    
    
    
(define FVAR-set-cdr!
    (lambda ()
        (string-append 
            "// FVAR set-cdr!"                                  NL
            "  JUMP(L_set_cdr_closure);"                        NL 
            
            "L_set_cdr_code:"                                   NL 
            "  PUSH(FP);"                                       NL
            "  MOV(FP, SP);"                                    NL
            
            "  CMP(FPARG(1), IMM(2));"                          NL
            "  JUMP_NE(L_closure_error_args_count);"            NL
            
            "  MOV(R0, FPARG(2));"                              NL
            "  CMP(INDD(R0, 0), IMM(T_PAIR));"                  NL
            "  JUMP_NE(L_error_set_cdr_not_pair);"              NL
            
            "  MOV(R1, FPARG(3));"                              NL
            "  MOV(INDD(R0,2),R1);"                             NL
            
            "  MOV(R0, SOB_VOID);"                              NL           

            "  POP(FP);"                                        NL
            "  RETURN;"                                         NL NL

            "L_set_cdr_closure:"                                NL
            (MALLOC-CLOSURE "L_set_cdr_code")
            "  MOV(IND(" (n->s (lookup-fvar-table 'set-cdr! *fvar-table*)) "), R0);" NL NL
        )
    ))
    
    


(define FVAR-numerator
    (lambda ()
        (string-append 
            "// FVAR numerator"                         NL
            "  JUMP(L_numerator_closure);"              NL 
            
            "L_numerator_code:"                         NL 
            "  PUSH(FP);"                               NL
            "  MOV(FP, SP);"                            NL

            "  CMP(FPARG(1), IMM(1));"                  NL
            "  JUMP_NE(L_closure_error_args_count);"    NL
            
            "  MOV(R1, FPARG(2));"                      NL
            "  CMP(INDD(R1, 0), IMM(T_INTEGER));"       NL
            "  JUMP_EQ(L_numerator_int);"               NL
            
            "  CMP(INDD(R1, 0), IMM(T_FRACTION));"      NL
            "  JUMP_NE(L_numerator_not_num);"           NL

            "L_numerator_int:"
                        
            "  PUSH(INDD(R1, 1));"                      NL
            "  CALL(MAKE_SOB_INTEGER);"                 NL
            "  DROP(1);"                                NL

            "  POP(FP);"                                NL
            "  RETURN;"                                 NL NL

            "L_numerator_closure:"                      NL
            (MALLOC-CLOSURE "L_numerator_code")
            "  MOV(IND(" (n->s (lookup-fvar-table 'numerator *fvar-table*)) "), R0);" NL NL
        )
    ))
    
    
(define FVAR-denominator
    (lambda ()
        (string-append 
            "// FVAR denominator"                         NL
            "  JUMP(L_denominator_closure);"              NL 
            
            "L_denominator_code:"                         NL 
            "  PUSH(FP);"                               NL
            "  MOV(FP, SP);"                            NL

            "  CMP(FPARG(1), IMM(1));"                  NL
            "  JUMP_NE(L_closure_error_args_count);"    NL
            
            "  MOV(R1, FPARG(2));"                      NL
            "  CMP(INDD(R1, 0), IMM(T_INTEGER));"       NL
            "  JUMP_EQ(L_denominator_int);"             NL
            
            "  CMP(INDD(R1, 0), IMM(T_FRACTION));"      NL
            "  JUMP_NE(L_denominator_not_num);"         NL
            
            "  PUSH(INDD(R1, 2));"                      NL
            "  CALL(MAKE_SOB_INTEGER);"                 NL
            "  DROP(1);"                                NL
            "  JUMP(L_numerator_end)"                   NL
            
            "L_denominator_int:"
            "  PUSH(IMM(1));"                           NL
            "  CALL(MAKE_SOB_INTEGER);"                 NL
            "  DROP(1);"                                NL

            "L_numerator_end:"                          NL
            "  POP(FP);"                                NL
            "  RETURN;"                                 NL NL

            "L_denominator_closure:"                      NL
            (MALLOC-CLOSURE "L_denominator_code")
            "  MOV(IND(" (n->s (lookup-fvar-table 'denominator *fvar-table*)) "), R0);" NL NL
        )
    ))
    
    
(define FVAR-number?
    (lambda ()
        (string-append
            "// FVAR number?"                            NL
            "  JUMP(L_number_closure);"                  NL
            
            "L_number_code:"                             NL
            "  PUSH(FP);"                                NL
            "  MOV(FP, SP);"                             NL
            
            "  CMP(FPARG(1), IMM(1));"                   NL
            "  JUMP_NE(L_closure_error_args_count);"     NL
            
            "  MOV(R1, FPARG(2));"                       NL
            "  CMP(INDD(R1, 0), IMM(T_INTEGER));"        NL
            "  JUMP_EQ(L_number_true);"                  NL
            
            "  CMP(INDD(R1, 0), IMM(T_FRACTION));"       NL
            "  JUMP_EQ(L_number_true);"                  NL

            "  MOV(R0, IMM(SOB_FALSE));"                 NL
            "  JUMP(L_number_end);"                      NL
            
            "L_number_true:"                             NL
            "  MOV(R0, IMM(SOB_TRUE));"                  NL
            
            "L_number_end:"                              NL
            "  POP(FP);"                                 NL
            "  RETURN;"                                  NL NL
            
            "L_number_closure:"                            NL
            (MALLOC-CLOSURE "L_number_code")
            "  MOV(IND(" (n->s (lookup-fvar-table 'number? *fvar-table*)) "), R0);" NL NL
        )
    ))

    
(define FVAR-rational?
    (lambda ()
        (string-append
            "// FVAR rational?"                          NL
            "  JUMP(L_rational_closure);"                NL
            
            "L_rational_code:"                           NL
            "  PUSH(FP);"                                NL
            "  MOV(FP, SP);"                             NL
            
            "  CMP(FPARG(1), IMM(1));"                   NL
            "  JUMP_NE(L_closure_error_args_count);"     NL
            
            "  MOV(R1, FPARG(2));"                       NL
            "  CMP(INDD(R1, 0), IMM(T_INTEGER));"        NL
            "  JUMP_EQ(L_rational_true);"                NL
            
            "  CMP(INDD(R1, 0), IMM(T_FRACTION));"       NL
            "  JUMP_EQ(L_rational_true);"                NL

            "  MOV(R0, IMM(SOB_FALSE));"                 NL
            "  JUMP(L_rational_end);"                    NL
            
            "L_rational_true:"                           NL
            "  MOV(R0, IMM(SOB_TRUE));"                  NL
            
            "L_rational_end:"                            NL
            "  POP(FP);"                                 NL
            "  RETURN;"                                  NL NL
            
            "L_rational_closure:"                            NL
            (MALLOC-CLOSURE "L_rational_code")
            "  MOV(IND(" (n->s (lookup-fvar-table 'rational? *fvar-table*)) "), R0);" NL NL
        )
    ))
    
(define FVAR-string-set!
    (lambda ()
        (string-append 
            "// FVAR string-set!"                       NL
            "  JUMP(L_string_set_closure);"             NL 
            
            "L_string_set_code:"                        NL 
            "  PUSH(FP);"                               NL
            "  MOV(FP, SP);"                            NL

            "  CMP(FPARG(1), IMM(3));"                  NL
            "  JUMP_NE(L_closure_error_args_count);"    NL
            
            "  MOV(R0,FPARG(2));" 
            "  CMP(INDD(R0, 0), IMM(T_STRING));"        NL
            "  JUMP_NE(L_string_set_1_not_string);"     NL

            "  MOV(R1,FPARG(3));"                       NL
            "  CMP(INDD(R1, 0), IMM(T_INTEGER));"       NL
            "  JUMP_NE(L_string_set_2_not_int);"        NL

            "  MOV(R1,INDD(R1,1));"                     NL
            "  ADD(R1,2);"                              NL

            "  MOV(R2,FPARG(4));"                       NL
            "  CMP(INDD(R2, 0), IMM(T_CHAR));"          NL
            "  JUMP_NE(L_string_set_3_not_char);"       NL

            "  MOV(R2,INDD(R2,1));"                     NL

            "  MOV(INDD(R0,R1),R2);"                    NL

            "  MOV(R0,SOB_VOID);"                       NL

            "  POP(FP);"                                NL
            "  RETURN;"                                 NL NL

            "L_string_set_closure:"                      NL
            (MALLOC-CLOSURE "L_string_set_code")
            "  MOV(IND(" (n->s (lookup-fvar-table 'string-set! *fvar-table*)) "), R0);" NL NL
        )
    ))
    
    
(define FVAR-vector-set!
    (lambda ()
        (string-append 
            "// FVAR vector-set!"                       NL
            "  JUMP(L_vector_set_closure);"             NL 
            
            "L_vector_set_code:"                        NL 
            "  PUSH(FP);"                               NL
            "  MOV(FP, SP);"                            NL

            "  CMP(FPARG(1), IMM(3));"                  NL
            "  JUMP_NE(L_closure_error_args_count);"    NL
            
            "  MOV(R0,FPARG(2));" 
            "  CMP(INDD(R0, 0), IMM(T_VECTOR));"        NL
            "  JUMP_NE(L_vector_set_1_not_vector);"     NL

            "  MOV(R1,FPARG(3));"                       NL
            "  CMP(INDD(R1, 0), IMM(T_INTEGER));"       NL
            "  JUMP_NE(L_vector_set_2_not_int);"       NL

            "  MOV(R1,INDD(R1,1));"                     NL
            "  ADD(R1,2);"                              NL

            "  MOV(R2,FPARG(4));"                       NL

            "  MOV(INDD(R0,R1),R2);"                    NL

            "  MOV(R0,SOB_VOID);"                       NL

            "  POP(FP);"                                NL
            "  RETURN;"                                 NL NL

            "L_vector_set_closure:"                      NL
            (MALLOC-CLOSURE "L_vector_set_code")
            "  MOV(IND(" (n->s (lookup-fvar-table 'vector-set! *fvar-table*)) "), R0);" NL NL
        )
    ))
    
    
(define FVAR-remainder
    (lambda ()
        (string-append 
            "// FVAR remainder"                       NL
            "  JUMP(L_remainder_closure);"             NL 
            
            "L_remainder_code:"                        NL 
            "  PUSH(FP);"                               NL
            "  MOV(FP, SP);"                            NL

            "  CMP(FPARG(1), IMM(2));"                  NL
            "  JUMP_NE(L_closure_error_args_count);"    NL
            
            "  MOV(R0,FPARG(2));" 
            "  CMP(INDD(R0, 0), IMM(T_INTEGER));"       NL
            "  JUMP_NE(L_remainder_not_integer);"       NL

            "  MOV(R1,FPARG(3));"                       NL
            "  CMP(INDD(R1, 0), IMM(T_INTEGER));"       NL
            "  JUMP_NE(L_remainder_not_integer);"       NL

            "  MOV(R0, INDD(R0,1));"                    NL
            "  MOV(R1, INDD(R1,1));"                    NL

            "  REM(R0, R1);"                            NL
            
            "  PUSH(R0);"                               NL
            "  CALL(MAKE_SOB_INTEGER);"                 NL
            "  DROP(1);"                                NL

            "  POP(FP);"                                NL
            "  RETURN;"                                 NL NL

            "L_remainder_closure:"                      NL
            (MALLOC-CLOSURE "L_remainder_code")
            "  MOV(IND(" (n->s (lookup-fvar-table 'remainder *fvar-table*)) "), R0);" NL NL
        )
    ))
    
    
(define FVAR-make-string
    (lambda ()
        (string-append 
            "// FVAR make-string"                       NL
            "  JUMP(L_make_string_closure);"            NL 
            
            "L_make_string_code:"                       NL 
            "  PUSH(FP);"                               NL
            "  MOV(FP, SP);"                            NL

            "  CMP(FPARG(1) ,IMM(1));"
            "  JUMP_LT(L_closure_error_args_count);"    NL

            "  MOV(R1, 0);"                             NL
            "  MOV(R2, FPARG(1));"                      NL
            "  CMP(R2, IMM(1));"                        NL
            "  JUMP_EQ(L_make_string_malloc);"          NL
            
            "  MOV(R1,FPARG(3));"                       NL
            "  MOV(R1,INDD(R1,1));"                     NL

            "L_make_string_malloc:"                     NL
            "  MOV(R2,FPARG(2));"                       NL
            "  MOV(R2,INDD(R2,1));"                     NL
            "  MOV(R3,R2);"                             NL
            "  ADD(R3,2);"                              NL
            
            "  PUSH(R3);"                               NL
            "  CALL(MALLOC);"                           NL
            "  DROP(1);"                                NL

            "  MOV(IND(R0),T_STRING);"                  NL
            "  MOV(INDD(R0,1),R2);"                     NL
            "  MOV(R3,2);"                              NL

            "L_make_string_loop:"                       NL
            "  CMP(R2,0);"                              NL
            "  JUMP_EQ(L_make_string_end);"             NL
            "  MOV(INDD(R0,R3),R1);"                    NL
            "  INCR(R3);"                               NL
            "  DECR(R2);"                               NL
            "  JUMP(L_make_string_loop);"               NL

            "  L_make_string_end:"
            "  POP(FP);"                                NL
            "  RETURN;"                                 NL NL

            "L_make_string_closure:"                    NL
            (MALLOC-CLOSURE "L_make_string_code")
            "  MOV(IND(" (n->s (lookup-fvar-table 'make-string *fvar-table*)) "), R0);" NL NL
        )
    ))
    

(define FVAR-not
    (lambda ()
        (string-append
            "// FVAR not"                                   NL
            "  JUMP(L_not_closure);"                        NL
            "L_not_code:"                                   NL
            
            "  PUSH(FP);"                                   NL
            "  MOV(FP, SP);"                                NL
            
            "  CMP(FPARG(1), IMM(1));"                      NL
            "  JUMP_NE(L_closure_error_args_count);"        NL
            
            "  MOV(R1, FPARG(2));"                          NL
            "  CMP(R1, IMM(SOB_FALSE));"                    NL
            "  JUMP_EQ(L_not_true);"                        NL
            
            "  MOV(R0, IMM(SOB_FALSE));"                    NL
            "  JUMP(L_not_end);"                            NL
            
            "L_not_true:"                                   NL
            "  MOV(R0, IMM(SOB_TRUE));"                     NL
            "L_not_end:"                                    NL
            "  POP(FP);"                                    NL
            "  RETURN;"                                     NL NL
            
            "L_not_closure:"                                NL
            (MALLOC-CLOSURE "L_not_code")
            "  MOV(IND(" (n->s (lookup-fvar-table 'not *fvar-table*)) "), R0);" NL NL
           
        )
    ))
    
    
(define FVAR-apply
    (lambda ()
        (string-append
            "// FVAR apply"                              NL
            "  JUMP(L_apply_closure);"                   NL
            "L_apply_code:"                              NL    

            "  PUSH(FP);"                                NL
            "  MOV(FP,SP);"                              NL

            "  MOV(R8,FPARG(1));"                        NL
            "  ADD(R8,3);"                               NL
            
            "  MOV(R3,FP);"                              NL
            
            "  SUB(R3,R8);"                              NL  ; R3 = last argument        
            "  SUB(R8,3);"                               NL  ; R8 = num of args

            "  MOV(R2,R3);"                              NL      
            
            "  MOV(R5,FPARG(-1));"                       NL ; R5 = return address  
            "  MOV(R6,FPARG(-2));"                       NL ; R6 = oldFP      
            
            "  MOV(R7,FPARG(2));"                        NL ; R7 = procedure address
            "  MOV(R1,FPARG(3));"                        NL ; R1 = list of args address
            
            "  MOV(R4,0);"                               NL ; R4 = i
            
        "COPY_LIST_TO_STACK:"                            NL
            "  CMP(IND(R1),T_NIL);"                      NL
            "  JUMP_EQ(COPY_LIST_TO_STACK_END);"         NL
            
            "  MOV(STACK(R3),INDD(R1,1));"               NL ; copy car of list to stack
            "  INCR(R4);"                                NL
            "  INCR(R3);"                                NL
            "  MOV(R1,INDD(R1,2));"                      NL ; R1 = cdr of list of args
            "  JUMP(COPY_LIST_TO_STACK);"                NL
            
        "COPY_LIST_TO_STACK_END:"                        NL
            "  MOV(R10,R3);"                             NL
            "  DECR(R3);"                                NL


        "INVERT_OPERANDS:"                               NL
            "  CMP(R3,R2);"                              NL
            "  JUMP_LE(APPLY_END);"                      NL
            
            "  MOV(R9,STACK(R2));"                       NL ;R9 = arg at top of stack 
            "  MOV(STACK(R2),STACK(R3));"                NL
            "  MOV(STACK(R3),R9);"                       NL
            "  INCR(R2);"                                NL
            "  DECR(R3);"                                NL
            "  JUMP(INVERT_OPERANDS);"                   NL
            
        "APPLY_END:"                                     NL
            "  MOV(STACK(R10),R4);"                      NL  ; copy numb of args to stack
            "  INCR(R10);"                               NL
            "  MOV(STACK(R10),INDD(R7,1));"              NL ; copy env to stack
            "  INCR(R10);"                               NL
            "  MOV(STACK(R10),R5);"                      NL; copy return address to stack
                    
            "  SUB(R4,R8);"                              NL
            "  ADD(SP,R4);"                              NL ; correct SP
            
            "  MOV(FP,R6);"                              NL; update oldFP
            
            "  JUMPA(INDD(R7,2));"                       NL ; apply procedure
    
            "L_apply_closure:"                           NL
            (MALLOC-CLOSURE "L_apply_code")
            "  MOV(IND(" (n->s (lookup-fvar-table 'apply *fvar-table*)) "), R0);" NL NL
        )
    ))
    
    
(define FVAR-vector
    (lambda ()
        (string-append
            "// FVAR vector"                NL
            "  JUMP(L_vector_var_closure);" NL
            "L_vector_var_code:"            NL    
                  
            "  PUSH(FP);"                   NL
            "  MOV(FP, SP);"                NL
            
            "  MOV(R1,FPARG(1));"           NL
            "  MOV(R2,IMM(2));"             NL
            
            "L_vector_args:"                NL
            "  CMP(R1,IMM(0));"             NL
            "  JUMP_EQ(L_vector_args_end);" NL
            "  MOV(R3,FPARG(R2));"          NL
            "  PUSH(R3);"                   NL
            "  DECR(R1);"                   NL
            "  INCR(R2);"                   NL
            "  JUMP(L_vector_args);"        NL
            
            "L_vector_args_end:"            NL
            "  MOV(R4, FPARG(1));"          NL
            "  PUSH(R4);"                   NL
            "  CALL(MAKE_SOB_VECTOR);"      NL
            
            "  POP(R4);"                    NL
            "  DROP(R4);"                   NL
           
            "  POP(FP);"                    NL
            "  RETURN;"                     NL
            
            "L_vector_var_closure:"         NL
            (MALLOC-CLOSURE "L_vector_var_code")
            "  MOV(IND(" (n->s (lookup-fvar-table 'vector *fvar-table*)) "), R0);" NL NL
        )
    )) 
    
    
(define CODE-GEN-FVARS
    (lambda() 
        (string-append 
            "// *** FVAR CODE ***"    NL
;;            (FVAR-append)          
            (FVAR-apply)
            (FVAR-<)
            (FVAR-=)
            (FVAR->)
            (FVAR-+)
            (FVAR-/)
            (FVAR-*)
            (FVAR--)
            (FVAR-boolean?)
            (FVAR-car)             
            (FVAR-cdr)              
            (FVAR-char->integer)
            (FVAR-char?)
            (FVAR-cons)             
            (FVAR-denominator)
;;             (FVAR-eq?)
            (FVAR-integer?)
            (FVAR-integer->char)
;;            (FVAR-list)            
            (FVAR-make-string)
             (FVAR-make-vector)
;            (FVAR-map)                      
            (FVAR-not)             
            (FVAR-null?)
            (FVAR-number?)
            (FVAR-numerator)
            (FVAR-pair?)
            (FVAR-procedure?)
            (FVAR-rational?)
            (FVAR-remainder)
            (FVAR-set-car!)
            (FVAR-set-cdr!)
            (FVAR-string-length)
            (FVAR-string-ref)
            (FVAR-string-set!)
;;             (FVAR-string->symbol)  
            (FVAR-string?)
;;             (FVAR-symbol?)
;;             (FVAR-symbol->string)
            (FVAR-vector)
            (FVAR-vector-length)
            (FVAR-vector-ref)
            (FVAR-vector-set!)
            (FVAR-vector?)
            (FVAR-zero?)
            "// *** FVAR CODE- END ***"    NL NL

        )
    ))
    
    

    