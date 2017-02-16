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

(define ^label-if-else (^^label "L_if3_else_"))

(define ^label-if-exit (^^label "L_if3_exit_"))
    
(define ^label-or-exit (^^label "L_or_exit_"))
    
(define ^label-applic-error-not-closure (^^label "L_error_cannot_apply_non_closure_"))

(define ^label-closure-body (^^label "L_closure_body_lambda_simple_"))

(define ^label-closure-exit (^^label "L_closure_exit_lambda_simple_"))

(define ^label-error-lambda-args-count (^^label "L_error_lambda_args_count_"))
   
(define ^label-closure-loop-copy-env (^^label "L_closure_loop_copy_env_lambda_simple_"))

(define ^label-closure-loop-copy-env-end (^^label "L_closure_loop_copy_env_end_lambda_simple_"))
    
(define ^label-closure-loop-copy-stack (^^label "L_closure_loop_copy_stack_lambda_simple_"))

(define ^label-closure-loop-copy-stack-end (^^label "L_closure_loop_copy_stack_end_lambda_simple_"))
    
(define ^label-fix-stack-loop-opt (^^label "L_fix_stack_loop_opt_"))

(define ^label-fix-stack-loop-opt-end (^^label "L_fix_stack_loop_opt_end_"))

(define ^label-closure-opt-code (^^label "L_closure_opt_code_"))
    
(define ^label-closure-opt-exit (^^label "L_closure_opt_exit_"))     
    
(define ^label-closure-opt-loop-copy (^^label "L_closure_opt_loop_copy_"))

(define ^label-closure-opt-loop-copy-end (^^label "L_closure_opt_loop_copy_end"))     

(define ^label-closure-opt-loop-copy-stack-end (^^label "L_closure_opt_loop_copy_stack_end_"))     

(define ^label-closure-opt-loop-copy-stack (^^label "L_closure_opt_loop_copy_stack_"))     

(define ^label-closure-opt-loop-create-list (^^label "L_closure_opt_loop_create_list_"))     

(define ^label-closure-opt-loop-create-list-end (^^label "L_closure_opt_loop_create_list_end_"))     

(define ^label-closure-var-code (^^label "L_closure_var_code_"))

(define ^label-closure-var-exit (^^label "L_closure_var_exit_"))
    
(define ^label-closure-var-loop-copy (^^label "L_closure_var_loop_copy_"))

(define ^label-closure-var-loop-copy-end (^^label "L_closure_var_loop_copy_end_"))

(define ^label-closure-var-loop-copy-stack-end (^^label "L_closure_var_loop_copy_stack_end_"))

(define ^label-closure-var-loop-copy-stack (^^label "L_closure_var_loop_copy_stack_"))

(define ^label-closure-var-loop-create-list (^^label "L_closure_var_loop_create_list_"))

(define ^label-closure-var-loop-create-list-end (^^label "L_closure_var_loop_create_list_end_"))
    
(define ^label-fix-stack-loop-var (^^label "L_fix_stack_loop_var_"))

(define ^label-fix-stack-loop-var-end (^^label "L_fix_stack_loop_var_end_"))

(define ^label-fix-stack-loop-var-end-loop (^^label "L_fix_stack_loop_var_end_loop_"))
