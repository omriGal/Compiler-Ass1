;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; list of lables for the compiler  ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ^^label
        (lambda (pre)
            (let ((num 0))
                    (lambda ()
                     (set! num (+ num 1))
                     (string-append pre (number->string num))))
    ))

(define label-if-else (^^label "L_if3_else"))

(define label-if-exit (^^label "L_if3_exit"))
    
(define label-or-exit (^^label "L_or_exit"))
    
(define label-error-not-closure (^^label "L_error_not_closure"))

(define label-closure-code (^^label "L_closure_code_lambda_simple"))

(define label-closure-exit (^^label "L_closure_exit_lambda_simple"))

(define label-closure-error (^^label "L_closure_error_lambda_simple"))
   
(define label-closure-loop-copy (^^label "L_closure_loop_copy_lambda_simple"))

(define label-closure-loop-copy-exit (^^label "L_closure_loop_copy_exit_lambda_simple"))
    
(define label-closure-loop-copy-stack (^^label "L_closure_loop_copy_stack_lambda_simple"))

(define label-closure-loop-stack-exit (^^label "L_closure_loop_copy_stack_exit_lambda_simple"))
    
(define label-error-lambda-args-count (^^label "L_error_lambda_args_count"))
    
(define label-fix-stack-loop-opt (^^label "L_fix_stack_loop_opt"))

(define label-fix-stack-loop-opt-end (^^label "L_fix_stack_loop_opt_end"))

(define label-closure-opt-code (^^label "L_closure_opt_code"))
    
(define label-closure-opt-exit (^^label "L_closure_opt_exit"))     
    
(define label-closure-opt-loop-copy (^^label "L_closure_opt_loop_copy"))     

(define label-closure-opt-loop-copy-end (^^label "L_closure_opt_loop_copy_end"))     

(define label-closure-opt-loop-copy-stack-end (^^label "L_closure_opt_loop_copy_stack_end"))     

(define label-closure-opt-loop-copy-stack (^^label "L_closure_opt_loop_copy_stack"))     

(define label-closure-opt-loop-create-list (^^label "L_closure_opt_loop_create_list"))     

(define label-closure-opt-loop-create-list-end (^^label "L_closure_opt_loop_create_list_end"))     

(define label-closure-var-code (^^label "L_closure_var_code"))

(define label-closure-var-exit (^^label "L_closure_var_exit"))
    
(define label-closure-var-loop-copy (^^label "L_closure_var_loop_copy"))

(define label-closure-var-loop-copy-end (^^label "L_closure_var_loop_copy_end"))

(define label-closure-var-loop-copy-stack-end (^^label "L_closure_var_loop_copy_stack_end"))

(define label-closure-var-loop-copy-stack (^^label "L_closure_var_loop_copy_stack"))

(define label-closure-var-loop-create-list (^^label "L_closure_var_loop_create_list"))

(define label-closure-var-loop-create-list-end (^^label "L_closure_var_loop_create_list_end"))
    
(define label-fix-stack-loop-var (^^label "L_fix_stack_loop_var"))

(define label-fix-stack-loop-var-end (^^label "L_fix_stack_loop_var_end"))

(define label-fix-stack-loop-var-end-loop (^^label "L_fix_stack_loop_var_end_loop"))
