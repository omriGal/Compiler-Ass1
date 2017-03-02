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

(define ^label-error-lambda-arg-count (^^label "L_error_lambda_args_count_"))
   
(define ^label-closure-loop-copy-env (^^label "L_closure_loop_copy_env_lambda_simple_"))

(define ^label-closure-loop-copy-env-end (^^label "L_closure_loop_copy_env_end_lambda_simple_"))
    
(define ^label-closure-loop-copy-stack (^^label "L_closure_loop_copy_stack_lambda_simple_"))

(define ^label-closure-loop-copy-stack-end (^^label "L_closure_loop_copy_stack_end_lambda_simple_"))
    
    
(define ^label-fix-stack-loop-opt (^^label "L_fix_stack_loop_opt_"))

(define ^label-fix-stack-loop-opt-end (^^label "L_fix_stack_loop_opt_end_"))


(define ^label-closure-lambda-opt-body (^^label "L_closure_lambda_opt_body_"))
    
(define ^label-closure-lambda-opt-end (^^label "L_closure_lambda_opt_end_"))

(define ^label-error-lambda-opt-arg-count (^^label "L_error_lambda_opt_arg_count_"))

    
(define ^label-closure-lambda-opt-loop-copy-env (^^label "L_closure_lambda_opt_loop_copy_env_"))

(define ^label-closure-lambda-opt-loop-copy-env-end (^^label "L_closure_lambda_opt_loop_copy_env_end"))     

(define ^label-closure-lambda-opt-loop-copy-stack (^^label "L_closure_lambda_opt_loop_copy_stack_"))     

(define ^label-closure-lambda-opt-loop-copy-stack-end (^^label "L_closure_lambda_opt_loop_copy_stack_end_"))     

(define ^label-closure-lambda-opt-loop-create-list (^^label "L_closure_lambda_opt_loop_create_list_"))     

(define ^label-closure-lambda-opt-loop-create-list-end (^^label "L_closure_lambda_opt_loop_create_list_end_"))     


(define ^label-closure-var-body (^^label "L_closure_lambda_var_body_"))

(define ^label-closure-var-end (^^label "L_closure_lambda_var_end_"))
;;     
(define ^label-closure-var-loop-copy-env (^^label "L_closure_lambda_var_loop_copy_"))

(define ^label-closure-var-loop-copy-env-end (^^label "L_closure_lambda_var_loop_copy_end_"))

(define ^label-closure-var-loop-copy-stack (^^label "L_closure_lambda_var_loop_copy_stack_"))

(define ^label-closure-var-loop-copy-stack-end (^^label "L_closure_lambda_var_loop_copy_stack_end_"))


(define ^label-closure-var-loop-create-list (^^label "L_closure_lambda_var_loop_create_list_"))

(define ^label-closure-var-loop-create-list-end (^^label "L_closure_lambda_var_loop_create_list_end_"))
    
(define ^label-tc-applic-move-stack (^^label "L_tc_applic_move_stack_"))


(define ^label-print-ans (^^label "L_print_ans_"))

