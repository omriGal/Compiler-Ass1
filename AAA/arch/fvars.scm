
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
    

(define *fvar-table*  (list `(car           3001)
                            `(cdr           3002)
                            `(cons          3003)           
                            `(boolean?      3004)
                            `(char?         3005)
                            `(integer?      3006)
                            `(null?         3007)
                            `(pair?         3008)
                            `(procedure?    3009)
                        ))
                        
(define lookup-fvar-table
    (lambda (fvar fvar-table)
        (if (null? fvar-table) 
            `(fvar ,fvar wasn't found in fvar table)
             (if (eq? fvar (get-fvar (car fvar-table)))
                        (fet-fvar-addr (car fvar-table))
                        (lookup-fvar-table fvar  (cdr fvar-table))))
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
            "  PUSH(IMM(3));"                                   NL
            "  CALL(MALLOC);"                                   NL
            "  DROP(1);"                                        NL
            "  MOV(INDD(R0, 0), IMM(T_CLOSURE));"               NL
            "  MOV(INDD(R0, 1), IMM(0));"                       NL
            "  MOV(INDD(R0, 2), LABEL(L_car_code));"            NL
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
            "  PUSH(IMM(3));"                           NL
            "  CALL(MALLOC);"                           NL
            "  DROP(1);"                                NL
            "  MOV(INDD(R0, 0), IMM(T_CLOSURE));"       NL
            "  MOV(INDD(R0, 1), IMM(0));"               NL
            "  MOV(INDD(R0, 2), LABEL(L_cdr_code));"    NL
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
            "  PUSH(IMM(3));"                         NL
            "  CALL(MALLOC);"                         NL
            "  DROP(1);"                              NL
            "  MOV(INDD(R0, 0), IMM(T_CLOSURE));"     NL
            "  MOV(INDD(R0, 1), IMM(0));"             NL
            "  MOV(INDD(R0, 2), LABEL(L_cons_code));" NL
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
            "  PUSH(IMM(3));"                               NL
            "  CALL(MALLOC);"                               NL
            "  DROP(1);"                                    NL
            "  MOV(INDD(R0, 0), IMM(T_CLOSURE));"           NL
            "  MOV(INDD(R0, 1), IMM(0));"                   NL
            "  MOV(INDD(R0, 2), LABEL(L_boolean_code));"    NL
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
            "  PUSH(IMM(3));"                               NL
            "  CALL(MALLOC);"                               NL
            "  DROP(1);"                                    NL
            "  MOV(INDD(R0, 0), IMM(T_CLOSURE));"           NL
            "  MOV(INDD(R0, 1), IMM(0));"                   NL
            "  MOV(INDD(R0, 2), LABEL(L_char_code));"       NL
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
            "  PUSH(IMM(3));"                               NL
            "  CALL(MALLOC);"                               NL
            "  DROP(1);"                                    NL
            "  MOV(INDD(R0, 0), IMM(T_CLOSURE));"           NL
            "  MOV(INDD(R0, 1), IMM(0));"                   NL
            "  MOV(INDD(R0, 2), LABEL(L_integer_code));"    NL
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
            "  PUSH(IMM(3));"                            NL
            "  CALL(MALLOC);"                            NL
            "  DROP(1);"                                 NL
            "  MOV(INDD(R0, 0), IMM(T_CLOSURE));"        NL
            "  MOV(INDD(R0, 1), IMM(0));"                NL
            "  MOV(INDD(R0, 2), LABEL(L_null_code));"    NL
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
            "  PUSH(IMM(3));"                            NL
            "  CALL(MALLOC);"                            NL
            "  DROP(1);"                                 NL
            "  MOV(INDD(R0, 0), IMM(T_CLOSURE));"        NL
            "  MOV(INDD(R0, 1), IMM(0));"                NL
            "  MOV(INDD(R0, 2), LABEL(L_pair_code));"    NL
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
            "  PUSH(IMM(3));"                               NL
            "  CALL(MALLOC);"                               NL
            "  DROP(1);"                                    NL
            "  MOV(INDD(R0, 0), IMM(T_CLOSURE));"           NL
            "  MOV(INDD(R0, 1), IMM(0));"                   NL
            "  MOV(INDD(R0, 2), LABEL(L_procedure_code));"    NL
            "  MOV(IND(" (n->s (lookup-fvar-table 'procedure? *fvar-table*)) "), R0);" NL NL
        )
    ))
    
    
(define CODE-GEN-FVARS
        (string-append 
            "// *** FVAR CODE ***"    NL

;;            (FVAR-append)          Scheme
;;             (FVAR-apply)
;;             (FVAR-<)
;;             (FVAR-=)
;;             (FVAR->)
;;             (FVAR-+)
;;             (FVAR-/)
;;             (FVAR-*)
;;             (FVAR--)
            (FVAR-boolean?)
            (FVAR-car)              ; V
            (FVAR-cdr)              ; V
;;             (FVAR-char->integer)
            (FVAR-char?)
            (FVAR-cons)             ; V
;;             (FVAR-denominator)
;;             (FVAR-eq?)
            (FVAR-integer?)
;;             (FVAR-integer->char)
;;            (FVAR-list)            Scheme
;;             (FVAR-make-string)
;;             (FVAR-make-vector)
;            (FVAR-map)             Scheme         
;            (FVAR-not)             Scheme
            (FVAR-null?)
;;             (FVAR-number?)
;;             (FVAR-numerator)
             (FVAR-pair?)
             (FVAR-procedure?)
;;             (FVAR-rational?)
;;             (FVAR-remainder)
;;             (FVAR-set-car!)
;;             (FVAR-set-cdr!)
;;             (FVAR-string-length)
;;             (FVAR-string-ref)
;;             (FVAR-string-set!)
;;             (FVAR-string->symbol)  
;;             (FVAR-string?)
;;             (FVAR-symbol?)
;;             (FVAR-symbol->string)
;;             (FVAR-vector)
;;             (FVAR-vector-length)
;;             (FVAR-vector-ref)
;;             (FVAR-vector-set!)
;;             (FVAR-vector?)
;;             (FVAR-zero?)
            "// *** FVAR CODE- END ***"    NL NL

        )
    )
    
    

    