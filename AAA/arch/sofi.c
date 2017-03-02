
#include <stdio.h>
#include <stdlib.h>

#define DO_SHOW 1

#include "cisc.h"
#include "INFO.h"


int main()
{
  START_MACHINE;


  JUMP(CONTINUE);

#include "char.lib"
#include "io.lib"
#include "math.lib"
#include "string.lib"
#include "system.lib" 
#include "scheme.lib" 

#define SOB_VOID	1
#define SOB_NIL 	2
#define SOB_TRUE 	3
#define SOB_FALSE 	5

  
 CONTINUE:

// *********************************
//          CONST TABLE 
// *********************************
  ADD(IND(0),IMM(54));
  MOV(IND(1),IMM(T_VOID));
  MOV(IND(2),IMM(T_NIL));
  MOV(IND(3),IMM(T_BOOL));
  MOV(IND(4),IMM(1));
  MOV(IND(5),IMM(T_BOOL));
  MOV(IND(6),IMM(0));
// *********************************
//          CONST TABLE  END
// *********************************


// *********************************
//          FVAR CODE 
// *********************************
// FVAR apply
  JUMP(L_apply_closure);
L_apply_code:
  PUSH(FP);
  MOV(FP,SP);
  MOV(R8,FPARG(1));
  ADD(R8,3);
  MOV(R3,FP);
  SUB(R3,R8);
  SUB(R8,3);
  MOV(R2,R3);
  MOV(R5,FPARG(-1));
  MOV(R6,FPARG(-2));
  MOV(R7,FPARG(2));
  MOV(R1,FPARG(3));
  MOV(R4,0);
COPY_LIST_TO_STACK:
  CMP(IND(R1),T_NIL);
  JUMP_EQ(COPY_LIST_TO_STACK_END);
  MOV(STACK(R3),INDD(R1,1));
  INCR(R4);
  INCR(R3);
  MOV(R1,INDD(R1,2));
  JUMP(COPY_LIST_TO_STACK);
COPY_LIST_TO_STACK_END:
  MOV(R10,R3);
  DECR(R3);
INVERT_OPERANDS:
  CMP(R3,R2);
  JUMP_LE(APPLY_END);
  MOV(R9,STACK(R2));
  MOV(STACK(R2),STACK(R3));
  MOV(STACK(R3),R9);
  INCR(R2);
  DECR(R3);
  JUMP(INVERT_OPERANDS);
APPLY_END:
  MOV(STACK(R10),R4);
  INCR(R10);
  MOV(STACK(R10),INDD(R7,1));
  INCR(R10);
  MOV(STACK(R10),R5);
  SUB(R4,R8);
  ADD(SP,R4);
  MOV(FP,R6);
  JUMPA(INDD(R7,2));
L_apply_closure:
  PUSH(IMM(3));
  CALL(MALLOC);
  DROP(1);
  MOV(INDD(R0, 0), IMM(T_CLOSURE));
  MOV(INDD(R0, 1), IMM(0));
  MOV(INDD(R0, 2), LABEL(L_apply_code));
  MOV(IND(7), R0);

// FVAR <
  JUMP(L_lower_closure);
L_lower_code:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R1,FPARG(1));
  CMP(R1, IMM(0));
  JUMP_EQ(L_closure_error_args_count);
  MOV(R0,SOB_TRUE);
  MOV(R2,IMM(0));
  CMP(R1,IMM(1));
  JUMP_EQ(L_lower_end);
  MOV(R3, FPARG(2));
  CMP(INDD(R3,0), IMM(T_FRACTION));
  JUMP_EQ(L_lower_before_loop);
  PUSH(IMM(1));
  PUSH(INDD(R3,1));
  CALL(MAKE_SOB_FRACTION);
  DROP(IMM(2));
  MOV(R3, R0);
L_lower_before_loop:
  MOV(R6, INDD(R3,1));
  MOV(R7, INDD(R3,2));
  INCR(R2);
L_lower_loop:
  CMP(R2, R1);
  MOV(R0,SOB_TRUE);
  JUMP_EQ(L_lower_end);
  MOV(R3, FPARG(R2+2));
  CMP(INDD(R3,0), IMM(T_FRACTION));
  JUMP_EQ(L_lower_fraction);
  PUSH(IMM(1));
  PUSH(INDD(R3,1));
  CALL(MAKE_SOB_FRACTION);
  DROP(2);
  MOV(R3, R0);
L_lower_fraction:
  MOV(R8, INDD(R3,1));
  MUL(R8, R7);
  MUL(R6, INDD(R3,2));
  CMP(R6,R8);
  MOV(R6,R8);
  MUL(R7, INDD(R3,2));
  INCR(R2);
  JUMP_LT(L_lower_loop);
  MOV(R0,SOB_FALSE);
L_lower_end:
  POP(FP);
  RETURN;

L_lower_closure:
  PUSH(IMM(3));
  CALL(MALLOC);
  DROP(1);
  MOV(INDD(R0, 0), IMM(T_CLOSURE));
  MOV(INDD(R0, 1), IMM(0));
  MOV(INDD(R0, 2), LABEL(L_lower_code));
  MOV(IND(8), R0);

// FVAR =
  JUMP(L_equal_closure);
L_equal_code:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R1,FPARG(1));
  CMP(R1, IMM(0));
  JUMP_EQ(L_closure_error_args_count);
  MOV(R0,SOB_TRUE);
  MOV(R2,IMM(0));
  CMP(R1,IMM(1));
  JUMP_EQ(L_equal_end);
  MOV(R3, FPARG(2));
  INCR(R2);
  CMP(INDD(R3,0), IMM(T_INTEGER));
  JUMP_EQ(L_equal_int_loop);
L_equal_fraction_loop:
  CMP(R2, R1);
  JUMP_EQ(L_equal_end);
  MOV(R4, FPARG(R2+2));  CMP(INDD(R4,0), IMM(T_FRACTION));
  JUMP_NE(L_equal_false);
  CMP(INDD(R3,1), INDD(R4,1));
  JUMP_NE(L_equal_false);
  CMP(INDD(R3,2), INDD(R4,2));
  JUMP_NE(L_equal_false);
  MOV(R3, R4);
  INCR(R2);
  JUMP_EQ(L_equal_fraction_loop);
L_equal_int_loop:
  CMP(R2, R1);
  JUMP_EQ(L_equal_end);
  MOV(R4, FPARG(R2+2));  CMP(INDD(R4,0), IMM(T_INTEGER));
  JUMP_NE(L_equal_false);
  CMP(INDD(R3,1), INDD(R4,1));
  MOV(R3, R4);
  INCR(R2);
  JUMP_EQ(L_equal_int_loop);
L_equal_false:
  MOV(R0,SOB_FALSE);
L_equal_end:
  POP(FP);
  RETURN; 

L_equal_closure:
  PUSH(IMM(3));
  CALL(MALLOC);
  DROP(1);
  MOV(INDD(R0, 0), IMM(T_CLOSURE));
  MOV(INDD(R0, 1), IMM(0));
  MOV(INDD(R0, 2), LABEL(L_equal_code));
  MOV(IND(9), R0);

// FVAR >
  JUMP(L_greater_closure);
L_greater_code:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R1,FPARG(1));
  CMP(R1, IMM(0));
  JUMP_EQ(L_closure_error_args_count);
  MOV(R0,SOB_TRUE);
  MOV(R2,IMM(0));
  CMP(R1,IMM(1));
  JUMP_EQ(L_greater_end);
  MOV(R3, FPARG(2));
  CMP(INDD(R3,0), IMM(T_FRACTION));
  JUMP_EQ(L_greater_before_loop);
  PUSH(IMM(1));
  PUSH(INDD(R3,1));
  CALL(MAKE_SOB_FRACTION);
  DROP(IMM(2));
  MOV(R3, R0);
L_greater_before_loop:
  MOV(R6, INDD(R3,1));
  MOV(R7, INDD(R3,2));
  INCR(R2);
L_greater_loop:
  CMP(R2, R1);
  MOV(R0,SOB_TRUE);
  JUMP_EQ(L_greater_end);
  MOV(R3, FPARG(R2+2));
  CMP(INDD(R3,0), IMM(T_FRACTION));
  JUMP_EQ(L_greater_fraction);
  PUSH(IMM(1));
  PUSH(INDD(R3,1));
  CALL(MAKE_SOB_FRACTION);
  DROP(2);
  MOV(R3, R0);
L_greater_fraction:
  MOV(R8, INDD(R3,1));
  MUL(R8, R7);
  MUL(R6, INDD(R3,2));
  CMP(R6,R8);
  MOV(R6,R8);  MUL(R7, INDD(R3,2));
  INCR(R2);
  JUMP_GT(L_greater_loop);
  MOV(R0,SOB_FALSE);
L_greater_end:
  POP(FP);
  RETURN;

L_greater_closure:
  PUSH(IMM(3));
  CALL(MALLOC);
  DROP(1);
  MOV(INDD(R0, 0), IMM(T_CLOSURE));
  MOV(INDD(R0, 1), IMM(0));
  MOV(INDD(R0, 2), LABEL(L_greater_code));
  MOV(IND(10), R0);

// FVAR +
  JUMP(L_plus_closure);
L_plus_code:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R1, FPARG(1));
  MOV(R2, IMM(0));
  MOV(R0, IMM(0));
  MOV(R6, IMM(0));
  MOV(R7, IMM(1));
  CMP(R1, IMM(0));
  JUMP_EQ(L_plus_end);
L_plus_loop:
  CMP(R1, R2);
  JUMP_EQ(L_plus_end);
  MOV(R3, FPARG(R2+2));
  CMP(INDD(R3,0), IMM(T_FRACTION));
  JUMP_EQ(L_plus_fraction);
  PUSH(IMM(1));
  PUSH(INDD(R3,1));
  CALL(MAKE_SOB_FRACTION);
  DROP(2);
  MOV(R3, R0);
L_plus_fraction:
  MOV(R8, INDD(R3,1));
  MUL(R8, R7);
  MUL(R6, INDD(R3,2));
  ADD(R6, R8);
  MUL(R7, INDD(R3,2));
  INCR(R2); 
  JUMP(L_plus_loop);
L_plus_end:
  PUSH(R7);
  PUSH(R6);
  CALL(GCD);
  DROP(IMM(2));
  MOV(R9, R0);
  PUSH(R7);
  PUSH(R6);
  CALL(MAKE_SOB_FRACTION);
  DROP(IMM(2));
  PUSH(R9);
  PUSH(R0);
  CALL(FIX_FRACTION);
  DROP(IMM(2));
  POP(FP);
  RETURN; 

L_plus_closure:
  PUSH(IMM(3));
  CALL(MALLOC);
  DROP(1);
  MOV(INDD(R0, 0), IMM(T_CLOSURE));
  MOV(INDD(R0, 1), IMM(0));
  MOV(INDD(R0, 2), LABEL(L_plus_code));
  MOV(IND(11), R0);

// FVAR /
  JUMP(L_div_closure);
L_div_code:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R1,FPARG(1));
  MOV(R2,0);
  MOV(R0,0);
  CMP(R1,IMM(0));
  JUMP_EQ(L_closure_error_args_count);
  CMP(R1,IMM(1));
  JUMP_EQ(L_div_1_arg);
  MOV(R3, FPARG(2));
  CMP(INDD(R3,0), IMM(T_FRACTION));
  JUMP_EQ(L_div_before_loop);
  PUSH(IMM(1));
  PUSH(INDD(R3,1));
  CALL(MAKE_SOB_FRACTION);
  DROP(IMM(2));
  MOV(R3, R0);
L_div_before_loop:
  MOV(R6, INDD(R3,1));
  MOV(R7, INDD(R3,2));
  INCR(R2);
L_div_loop:
  CMP(R1,R2);
  JUMP_EQ(L_div_end);
  MOV(R3, FPARG(R2+2));
  CMP(INDD(R3,0), IMM(T_FRACTION));
  JUMP_EQ(L_div_fraction);
  PUSH(IMM(1));
  PUSH(INDD(R3,1));
  CALL(MAKE_SOB_FRACTION);
  DROP(2);
  MOV(R3, R0);
L_div_fraction:
  CMP(IMM(0),INDD(R3,1));
  JUMP_EQ(L_division_by_zero);
  MUL(R6,INDD(R3,2));
  MUL(R7,INDD(R3,1));
  PUSH(R7);
  PUSH(R6);
  CALL(MAKE_SOB_FRACTION);
  DROP(IMM(2));
  MOV(R3, R0);
  PUSH(R1);
  PUSH(R2);
  PUSH(R3);
  CALL(FIX_DIV_FRACTION);
  DROP(IMM(1));
  POP(R2);
  POP(R1);
  MOV(R6, INDD(R0,1));
  MOV(R7, INDD(R0,2));
  INCR(R2);
  JUMP(L_div_loop);
L_div_1_arg:
  MOV(R3, FPARG(2));
  CMP(INDD(R3,0),IMM(T_FRACTION));
  JUMP_EQ(L_div_1_arg_fraction);
  PUSH(IMM(1));
  PUSH(INDD(R3,1));
  CALL(MAKE_SOB_FRACTION);
  DROP(IMM(2));
  MOV(R3, R0);
L_div_1_arg_fraction:
  PUSH(INDD(R3,1));
  PUSH(INDD(R3,2));
  CALL(MAKE_SOB_FRACTION);
  DROP(IMM(2));
  MOV(R3, R0);
  PUSH(R3);
  CALL(FIX_DIV_FRACTION);
  DROP(IMM(1));
  MOV(R6, INDD(R0,1));
  MOV(R7, INDD(R0,2));
L_div_end:
  PUSH(R7);
  PUSH(R6);
  CALL(GCD);
  DROP(IMM(2));
  MOV(R9, R0);
  PUSH(R7);
  PUSH(R6);
  CALL(MAKE_SOB_FRACTION);
  DROP(IMM(2));
  PUSH(R9);
  PUSH(R0);
  CALL(FIX_FRACTION);
  DROP(IMM(2));
  POP(FP);
  RETURN; 

L_div_closure:
  PUSH(IMM(3));
  CALL(MALLOC);
  DROP(1);
  MOV(INDD(R0, 0), IMM(T_CLOSURE));
  MOV(INDD(R0, 1), IMM(0));
  MOV(INDD(R0, 2), LABEL(L_div_code));
  MOV(IND(12), R0);

// FVAR *
  JUMP(L_mul_closure);
L_mul_code:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R1,FPARG(1));
  MOV(R2,0);
  MOV(R0,1);
  MOV(R6, IMM(1));
  MOV(R7, IMM(1));
  CMP(R1,IMM(0));
  JUMP_EQ(L_mul_end);
L_mul_loop:
  CMP(R1,R2);
  JUMP_EQ(L_mul_end);
  MOV(R3, FPARG(R2+2));
  CMP(INDD(R3,0), IMM(T_FRACTION));
  JUMP_EQ(L_mul_fraction);
  PUSH(IMM(1));
  PUSH(INDD(R3,1));
  CALL(MAKE_SOB_FRACTION);
  DROP(2);
  MOV(R3, R0);
L_mul_fraction:
  MUL(R6, INDD(R3,1));
  MUL(R7, INDD(R3,2));
  INCR(R2); 
  JUMP(L_mul_loop);
L_mul_end:
  PUSH(R7);
  PUSH(R6);
  CALL(GCD);
  DROP(IMM(2));
  MOV(R9, R0);
  PUSH(R7);
  PUSH(R6);
  CALL(MAKE_SOB_FRACTION);
  DROP(IMM(2));
  PUSH(R9);
  PUSH(R0);
  CALL(FIX_FRACTION);
  DROP(IMM(2));
  POP(FP);
  RETURN; 

L_mul_closure:
  PUSH(IMM(3));
  CALL(MALLOC);
  DROP(1);
  MOV(INDD(R0, 0), IMM(T_CLOSURE));
  MOV(INDD(R0, 1), IMM(0));
  MOV(INDD(R0, 2), LABEL(L_mul_code));
  MOV(IND(13), R0);

// FVAR -
  JUMP(L_minus_closure);
L_minus_code:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R1,FPARG(1));
  MOV(R2,0);
  MOV(R0,0);
  CMP(R1,IMM(0));
  JUMP_EQ(L_closure_error_args_count);
  CMP(R1, IMM(1));
  JUMP_EQ(L_minus_change_sign);
  MOV(R3, FPARG(2));
  CMP(INDD(R3,0), IMM(T_FRACTION));
  JUMP_EQ(L_minus_before_loop);
  PUSH(IMM(1));
  PUSH(INDD(R3,1));
  CALL(MAKE_SOB_FRACTION);
  DROP(IMM(2));
  MOV(R3, R0);
L_minus_before_loop:
  MOV(R6, INDD(R3,1));
  MOV(R7, INDD(R3,2));
  INCR(R2);
L_minus_loop:
  CMP(R1,R2);
  JUMP_EQ(L_minus_end);
  MOV(R3, FPARG(R2+2));
  CMP(INDD(R3,0), IMM(T_FRACTION));
  JUMP_EQ(L_minus_fraction);
  PUSH(IMM(1));
  PUSH(INDD(R3,1));
  CALL(MAKE_SOB_FRACTION);
  DROP(2);
  MOV(R3, R0);
L_minus_fraction:
  MOV(R8, INDD(R3,1));
  MUL(R8, R7);
  MUL(R6, INDD(R3,2));
  SUB(R6, R8);
  MUL(R7, INDD(R3,2));
  INCR(R2);
  JUMP(L_minus_loop);
L_minus_change_sign:
  MOV(R3, FPARG(2));
  CMP(INDD(R3,0),IMM(T_FRACTION));
  JUMP_EQ(L_minus_change_sign_fraction);
  PUSH(IMM(1));
  PUSH(INDD(R3,1));
  CALL(MAKE_SOB_FRACTION);
  DROP(IMM(2));
  MOV(R3, R0);
L_minus_change_sign_fraction:
  MOV(R6, INDD(R3,1));
  MUL(R6, IMM(-1));
  MOV(R7, INDD(R3,2));
L_minus_end:
  PUSH(R7);
  PUSH(R6);
  CALL(GCD);
  DROP(IMM(2));
  MOV(R9, R0);
  PUSH(R7);
  PUSH(R6);
  CALL(MAKE_SOB_FRACTION);
  DROP(IMM(2));
  PUSH(R9);
  PUSH(R0);
  CALL(FIX_FRACTION);
  DROP(IMM(2));
  POP(FP);
  RETURN; 

L_minus_closure:
  PUSH(IMM(3));
  CALL(MALLOC);
  DROP(1);
  MOV(INDD(R0, 0), IMM(T_CLOSURE));
  MOV(INDD(R0, 1), IMM(0));
  MOV(INDD(R0, 2), LABEL(L_minus_code));
  MOV(IND(14), R0);

// FVAR-boolean?
  JUMP(L_boolean_closure);
L_boolean_code:
  PUSH(FP);
  MOV(FP, SP);
  CMP(FPARG(1), IMM(1));
  JUMP_NE(L_closure_error_args_count);
  MOV(R1, FPARG(2));
  CMP(INDD(R1, 0), IMM(T_BOOL));
  JUMP_EQ(L_boolean_true);
  MOV(R0, IMM(SOB_FALSE));
  JUMP(L_boolean_end);
L_boolean_true:
  MOV(R0, IMM(SOB_TRUE));
L_boolean_end:
  POP(FP);
  RETURN;

L_boolean_closure:
  PUSH(IMM(3));
  CALL(MALLOC);
  DROP(1);
  MOV(INDD(R0, 0), IMM(T_CLOSURE));
  MOV(INDD(R0, 1), IMM(0));
  MOV(INDD(R0, 2), LABEL(L_boolean_code));
  MOV(IND(15), R0);

// FVAR-car
  JUMP(L_car_closure);
L_car_code:
  PUSH(FP);
  MOV(FP, SP);
  CMP(FPARG(1), IMM(1));
  JUMP_NE(L_closure_error_args_count);
  MOV(R1, FPARG(2));
  CMP(INDD(R1, 0), IMM(T_PAIR));
  JUMP_NE(L_error_car_not_pair);
  MOV(R0, INDD(R1, 1));
  POP(FP);
  RETURN;

L_car_closure:
  PUSH(IMM(3));
  CALL(MALLOC);
  DROP(1);
  MOV(INDD(R0, 0), IMM(T_CLOSURE));
  MOV(INDD(R0, 1), IMM(0));
  MOV(INDD(R0, 2), LABEL(L_car_code));
  MOV(IND(16), R0);

// FVAR-cdr
  JUMP(L_cdr_closure);
L_cdr_code:
  PUSH(FP);
  MOV(FP, SP);
  CMP(FPARG(1), IMM(1));
  JUMP_NE(L_closure_error_args_count);
  MOV(R1, FPARG(2));
  CMP(INDD(R1, 0), IMM(T_PAIR));
  JUMP_NE(L_error_cdr_not_pair);
  MOV(R0, INDD(R1, 2));
  POP(FP);
  RETURN;

L_cdr_closure:
  PUSH(IMM(3));
  CALL(MALLOC);
  DROP(1);
  MOV(INDD(R0, 0), IMM(T_CLOSURE));
  MOV(INDD(R0, 1), IMM(0));
  MOV(INDD(R0, 2), LABEL(L_cdr_code));
  MOV(IND(17), R0);

// FVAR char->integer
  JUMP(L_char2int_closure);
L_char2int_code:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R1,FPARG(1));
  CMP(R1,IMM(1));
  JUMP_NE(L_closure_error_args_count);
  MOV(R0, FPARG(2));
  CMP(INDD(R0, 0), IMM(T_CHAR));
  JUMP_NE(L_char2int_not_char);
  MOV(R0, INDD(R0,1));
  PUSH(R0);
  CALL(MAKE_SOB_INTEGER);
  DROP(IMM(1));
  POP(FP);
  RETURN;

L_char2int_closure:
  PUSH(IMM(3));
  CALL(MALLOC);
  DROP(1);
  MOV(INDD(R0, 0), IMM(T_CLOSURE));
  MOV(INDD(R0, 1), IMM(0));
  MOV(INDD(R0, 2), LABEL(L_char2int_code));
  MOV(IND(18), R0);

// FVAR-char?
  JUMP(L_char_closure);
L_char_code:
  PUSH(FP);
  MOV(FP, SP);
  CMP(FPARG(1), IMM(1));
  JUMP_NE(L_closure_error_args_count);
  MOV(R1, FPARG(2));
  CMP(INDD(R1, 0), IMM(T_CHAR));
  JUMP_EQ(L_char_true);
  MOV(R0, IMM(SOB_FALSE));
  JUMP(L_char_end);
L_char_true:
  MOV(R0, IMM(SOB_TRUE));
L_char_end:
  POP(FP);
  RETURN;

L_char_closure:
  PUSH(IMM(3));
  CALL(MALLOC);
  DROP(1);
  MOV(INDD(R0, 0), IMM(T_CLOSURE));
  MOV(INDD(R0, 1), IMM(0));
  MOV(INDD(R0, 2), LABEL(L_char_code));
  MOV(IND(19), R0);

// FVAR-cons
  JUMP(L_cons_closure);
L_cons_code:
  PUSH(FP);
  MOV(FP, SP);
  CMP(FPARG(1), IMM(2));
  JUMP_NE(L_closure_error_args_count);
  PUSH(FPARG(3));
  PUSH(FPARG(2));
  CALL(MAKE_SOB_PAIR);
  DROP(2);
  POP(FP);
  RETURN;

L_cons_closure:
  PUSH(IMM(3));
  CALL(MALLOC);
  DROP(1);
  MOV(INDD(R0, 0), IMM(T_CLOSURE));
  MOV(INDD(R0, 1), IMM(0));
  MOV(INDD(R0, 2), LABEL(L_cons_code));
  MOV(IND(20), R0);

// FVAR denominator
  JUMP(L_denominator_closure);
L_denominator_code:
  PUSH(FP);
  MOV(FP, SP);
  CMP(FPARG(1), IMM(1));
  JUMP_NE(L_closure_error_args_count);
  MOV(R1, FPARG(2));
  CMP(INDD(R1, 0), IMM(T_INTEGER));
  JUMP_EQ(L_denominator_int);
  CMP(INDD(R1, 0), IMM(T_FRACTION));
  JUMP_NE(L_denominator_not_num);
  PUSH(INDD(R1, 2));
  CALL(MAKE_SOB_INTEGER);
  DROP(1);
  JUMP(L_numerator_end)
L_denominator_int:  PUSH(IMM(1));
  CALL(MAKE_SOB_INTEGER);
  DROP(1);
L_numerator_end:
  POP(FP);
  RETURN;

L_denominator_closure:
  PUSH(IMM(3));
  CALL(MALLOC);
  DROP(1);
  MOV(INDD(R0, 0), IMM(T_CLOSURE));
  MOV(INDD(R0, 1), IMM(0));
  MOV(INDD(R0, 2), LABEL(L_denominator_code));
  MOV(IND(21), R0);

// FVAR string->symbol
  JUMP(L_eq_closure);
L_eq_code:
  PUSH(FP);
  MOV(FP, SP);
  CMP(FPARG(1), IMM(2));
  JUMP_NE(L_closure_error_args_count);
  MOV(R0, IMM(SOB_TRUE));
  MOV(R1, FPARG(2));
  MOV(R2, FPARG(3));
  MOV(R3,IND(R1));
  MOV(R4,IND(R2));
  CMP(R3, R4);
  JUMP_NE(L_eq_false);
  CMP(R3, IMM(T_NIL));
  JUMP_EQ(L_eq_end);
  CMP(R3, IMM(T_VOID));
  JUMP_EQ(L_eq_end);
  CMP(R3,IMM(T_INTEGER));
  JUMP_EQ(L_eq_value);
  CMP(R3,IMM(T_SYMBOL));
  JUMP_EQ(L_eq_value);
  CMP(R3,IMM(T_CHAR));
  JUMP_EQ(L_eq_value);
  CMP(R3, IMM(T_FRACTION));
  JUMP_EQ(L_eq_value);
  CMP(R3, IMM(T_BOOL));
  JUMP_EQ(L_eq_addr);
  CMP(R3, IMM(T_STRING));
  JUMP_EQ(L_eq_addr);
  CMP(R3, IMM(T_PAIR));
  JUMP_EQ(L_eq_addr);
  CMP(R3, IMM(T_CLOSURE));
  JUMP_EQ(L_eq_addr);
  CMP(R3, IMM(T_VECTOR));
  JUMP_EQ(L_eq_addr);
L_eq_value:
  CMP(INDD(R1, 1), INDD(R2, 1));
  JUMP_NE(L_eq_false);
  CMP(R3, IMM(T_FRACTION));
  JUMP_NE(L_eq_end);
  CMP(INDD(R1, 2), INDD(R2, 2));
  JUMP_NE(L_eq_false);
  JUMP(L_eq_end);
L_eq_addr:
  CMP(R1, R2);
  JUMP_NE(L_eq_false);
  JUMP(L_eq_end);
L_eq_false:
  MOV(R0, IMM(SOB_FALSE));
L_eq_end:
  POP(FP);
  RETURN;
L_eq_closure:
  PUSH(IMM(3));
  CALL(MALLOC);
  DROP(1);
  MOV(INDD(R0, 0), IMM(T_CLOSURE));
  MOV(INDD(R0, 1), IMM(0));
  MOV(INDD(R0, 2), LABEL(L_eq_code));
  MOV(IND(22), R0);

// FVAR-integer?
  JUMP(L_integer_closure);
L_integer_code:
  PUSH(FP);
  MOV(FP, SP);
  CMP(FPARG(1), IMM(1));
  JUMP_NE(L_closure_error_args_count);
  MOV(R1, FPARG(2));
  CMP(INDD(R1, 0), IMM(T_INTEGER));
  JUMP_EQ(L_integer_true);
  MOV(R0, IMM(SOB_FALSE));
  JUMP(L_integer_end);
L_integer_true:
  MOV(R0, IMM(SOB_TRUE));
L_integer_end:
  POP(FP);
  RETURN;

L_integer_closure:
  PUSH(IMM(3));
  CALL(MALLOC);
  DROP(1);
  MOV(INDD(R0, 0), IMM(T_CLOSURE));
  MOV(INDD(R0, 1), IMM(0));
  MOV(INDD(R0, 2), LABEL(L_integer_code));
  MOV(IND(23), R0);

// FVAR integer->char
  JUMP(L_int2char_closure);
L_int2char_code:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R1,FPARG(1));
  CMP(R1,IMM(1));
  JUMP_NE(L_closure_error_args_count);
  MOV(R0, FPARG(2));
  CMP(INDD(R0, 0), IMM(T_INTEGER));
  JUMP_NE(L_int2char_not_int);
  MOV(R0, INDD(R0,1));
  PUSH(R0);
  CALL(MAKE_SOB_CHAR);
  DROP(IMM(1));
  POP(FP);
  RETURN;

L_int2char_closure:
  PUSH(IMM(3));
  CALL(MALLOC);
  DROP(1);
  MOV(INDD(R0, 0), IMM(T_CLOSURE));
  MOV(INDD(R0, 1), IMM(0));
  MOV(INDD(R0, 2), LABEL(L_int2char_code));
  MOV(IND(24), R0);

// FVAR make-string
  JUMP(L_make_string_closure);
L_make_string_code:
  PUSH(FP);
  MOV(FP, SP);
  CMP(FPARG(1) ,IMM(1));  JUMP_LT(L_closure_error_args_count);
  MOV(R1, 0);
  MOV(R2, FPARG(1));
  CMP(R2, IMM(1));
  JUMP_EQ(L_make_string_malloc);
  MOV(R1,FPARG(3));
  MOV(R1,INDD(R1,1));
L_make_string_malloc:
  MOV(R2,FPARG(2));
  MOV(R2,INDD(R2,1));
  MOV(R3,R2);
  ADD(R3,2);
  PUSH(R3);
  CALL(MALLOC);
  DROP(1);
  MOV(IND(R0),T_STRING);
  MOV(INDD(R0,1),R2);
  MOV(R3,2);
L_make_string_loop:
  CMP(R2,0);
  JUMP_EQ(L_make_string_end);
  MOV(INDD(R0,R3),R1);
  INCR(R3);
  DECR(R2);
  JUMP(L_make_string_loop);
  L_make_string_end:  POP(FP);
  RETURN;

L_make_string_closure:
  PUSH(IMM(3));
  CALL(MALLOC);
  DROP(1);
  MOV(INDD(R0, 0), IMM(T_CLOSURE));
  MOV(INDD(R0, 1), IMM(0));
  MOV(INDD(R0, 2), LABEL(L_make_string_code));
  MOV(IND(25), R0);

// FVAR make-vector
  JUMP(L_make_vector_closure);
L_make_vector_code:
  PUSH(FP);
  MOV(FP, SP);
  CMP(FPARG(1), IMM(1));
  JUMP_NE(L_make_vector_2_args)
  MOV(R1, FPARG(2));
  CMP(INDD(R1, 0), IMM(T_INTEGER));
  JUMP_NE(L_make_vector_not_integer);
  PUSH(IMM(0));
  CALL(MAKE_SOB_INTEGER);
  DROP(1);
  MOV(R2, R0);
  JUMP(L_make_vector_loop);
L_make_vector_2_args:
  CMP(FPARG(1), IMM(2));
  JUMP_NE(L_closure_error_args_count);
  MOV(R1, FPARG(2));
  CMP(INDD(R1, 0), IMM(T_INTEGER));
  JUMP_NE(L_make_vector_not_integer);
  MOV(R2, FPARG(3));
L_make_vector_loop:
  MOV(R3, INDD(R1,1));
L_make_vector_loop_1:
  CMP(R3,IMM(0));
  JUMP_EQ(L_make_vector_loop_end);
  PUSH(R2);
  DECR(R3);
  JUMP(L_make_vector_loop_1);
L_make_vector_loop_end:  PUSH(INDD(R1,1));
  CALL(MAKE_SOB_VECTOR);
  DROP(INDD(R1,1));
  DROP(IMM(1));
  POP(FP);
  RETURN;

L_make_vector_closure:
  PUSH(IMM(3));
  CALL(MALLOC);
  DROP(1);
  MOV(INDD(R0, 0), IMM(T_CLOSURE));
  MOV(INDD(R0, 1), IMM(0));
  MOV(INDD(R0, 2), LABEL(L_make_vector_code));
  MOV(IND(26), R0);

// FVAR not
  JUMP(L_not_closure);
L_not_code:
  PUSH(FP);
  MOV(FP, SP);
  CMP(FPARG(1), IMM(1));
  JUMP_NE(L_closure_error_args_count);
  MOV(R1, FPARG(2));
  CMP(R1, IMM(SOB_FALSE));
  JUMP_EQ(L_not_true);
  MOV(R0, IMM(SOB_FALSE));
  JUMP(L_not_end);
L_not_true:
  MOV(R0, IMM(SOB_TRUE));
L_not_end:
  POP(FP);
  RETURN;

L_not_closure:
  PUSH(IMM(3));
  CALL(MALLOC);
  DROP(1);
  MOV(INDD(R0, 0), IMM(T_CLOSURE));
  MOV(INDD(R0, 1), IMM(0));
  MOV(INDD(R0, 2), LABEL(L_not_code));
  MOV(IND(27), R0);

// FVAR-null?
  JUMP(L_null_closure);
L_null_code:
  PUSH(FP);
  MOV(FP, SP);
  CMP(FPARG(1), IMM(1));
  JUMP_NE(L_closure_error_args_count);
  MOV(R1, FPARG(2));
  CMP(INDD(R1, 0), IMM(T_NIL));
  JUMP_EQ(L_null_true);
  MOV(R0, IMM(SOB_FALSE));
  JUMP(L_null_end);
L_null_true:
  MOV(R0, IMM(SOB_TRUE));
L_null_end:
  POP(FP);
  RETURN;

L_null_closure:
  PUSH(IMM(3));
  CALL(MALLOC);
  DROP(1);
  MOV(INDD(R0, 0), IMM(T_CLOSURE));
  MOV(INDD(R0, 1), IMM(0));
  MOV(INDD(R0, 2), LABEL(L_null_code));
  MOV(IND(28), R0);

// FVAR number?
  JUMP(L_number_closure);
L_number_code:
  PUSH(FP);
  MOV(FP, SP);
  CMP(FPARG(1), IMM(1));
  JUMP_NE(L_closure_error_args_count);
  MOV(R1, FPARG(2));
  CMP(INDD(R1, 0), IMM(T_INTEGER));
  JUMP_EQ(L_number_true);
  CMP(INDD(R1, 0), IMM(T_FRACTION));
  JUMP_EQ(L_number_true);
  MOV(R0, IMM(SOB_FALSE));
  JUMP(L_number_end);
L_number_true:
  MOV(R0, IMM(SOB_TRUE));
L_number_end:
  POP(FP);
  RETURN;

L_number_closure:
  PUSH(IMM(3));
  CALL(MALLOC);
  DROP(1);
  MOV(INDD(R0, 0), IMM(T_CLOSURE));
  MOV(INDD(R0, 1), IMM(0));
  MOV(INDD(R0, 2), LABEL(L_number_code));
  MOV(IND(29), R0);

// FVAR numerator
  JUMP(L_numerator_closure);
L_numerator_code:
  PUSH(FP);
  MOV(FP, SP);
  CMP(FPARG(1), IMM(1));
  JUMP_NE(L_closure_error_args_count);
  MOV(R1, FPARG(2));
  CMP(INDD(R1, 0), IMM(T_INTEGER));
  JUMP_EQ(L_numerator_int);
  CMP(INDD(R1, 0), IMM(T_FRACTION));
  JUMP_NE(L_numerator_not_num);
L_numerator_int:  PUSH(INDD(R1, 1));
  CALL(MAKE_SOB_INTEGER);
  DROP(1);
  POP(FP);
  RETURN;

L_numerator_closure:
  PUSH(IMM(3));
  CALL(MALLOC);
  DROP(1);
  MOV(INDD(R0, 0), IMM(T_CLOSURE));
  MOV(INDD(R0, 1), IMM(0));
  MOV(INDD(R0, 2), LABEL(L_numerator_code));
  MOV(IND(30), R0);

// FVAR-pair?
  JUMP(L_pair_closure);
L_pair_code:
  PUSH(FP);
  MOV(FP, SP);
  CMP(FPARG(1), IMM(1));
  JUMP_NE(L_closure_error_args_count);
  MOV(R1, FPARG(2));
  CMP(INDD(R1, 0), IMM(T_PAIR));
  JUMP_EQ(L_pair_true);
  MOV(R0, IMM(SOB_FALSE));
  JUMP(L_pair_end);
L_pair_true:
  MOV(R0, IMM(SOB_TRUE));
L_pair_end:
  POP(FP);
  RETURN;

L_pair_closure:
  PUSH(IMM(3));
  CALL(MALLOC);
  DROP(1);
  MOV(INDD(R0, 0), IMM(T_CLOSURE));
  MOV(INDD(R0, 1), IMM(0));
  MOV(INDD(R0, 2), LABEL(L_pair_code));
  MOV(IND(31), R0);

// FVAR-procedure?
  JUMP(L_procedure_closure);
L_procedure_code:
  PUSH(FP);
  MOV(FP, SP);
  CMP(FPARG(1), IMM(1));
  JUMP_NE(L_closure_error_args_count);
  MOV(R1, FPARG(2));
  CMP(INDD(R1, 0), IMM(T_CLOSURE));
  JUMP_EQ(L_procedure_true);
  MOV(R0, IMM(SOB_FALSE));
  JUMP(L_procedure_end);
L_procedure_true:
  MOV(R0, IMM(SOB_TRUE));
L_procedure_end:
  POP(FP);
  RETURN;

L_procedure_closure:
  PUSH(IMM(3));
  CALL(MALLOC);
  DROP(1);
  MOV(INDD(R0, 0), IMM(T_CLOSURE));
  MOV(INDD(R0, 1), IMM(0));
  MOV(INDD(R0, 2), LABEL(L_procedure_code));
  MOV(IND(32), R0);

// FVAR rational?
  JUMP(L_rational_closure);
L_rational_code:
  PUSH(FP);
  MOV(FP, SP);
  CMP(FPARG(1), IMM(1));
  JUMP_NE(L_closure_error_args_count);
  MOV(R1, FPARG(2));
  CMP(INDD(R1, 0), IMM(T_INTEGER));
  JUMP_EQ(L_rational_true);
  CMP(INDD(R1, 0), IMM(T_FRACTION));
  JUMP_EQ(L_rational_true);
  MOV(R0, IMM(SOB_FALSE));
  JUMP(L_rational_end);
L_rational_true:
  MOV(R0, IMM(SOB_TRUE));
L_rational_end:
  POP(FP);
  RETURN;

L_rational_closure:
  PUSH(IMM(3));
  CALL(MALLOC);
  DROP(1);
  MOV(INDD(R0, 0), IMM(T_CLOSURE));
  MOV(INDD(R0, 1), IMM(0));
  MOV(INDD(R0, 2), LABEL(L_rational_code));
  MOV(IND(33), R0);

// FVAR remainder
  JUMP(L_remainder_closure);
L_remainder_code:
  PUSH(FP);
  MOV(FP, SP);
  CMP(FPARG(1), IMM(2));
  JUMP_NE(L_closure_error_args_count);
  MOV(R0,FPARG(2));  CMP(INDD(R0, 0), IMM(T_INTEGER));
  JUMP_NE(L_remainder_not_integer);
  MOV(R1,FPARG(3));
  CMP(INDD(R1, 0), IMM(T_INTEGER));
  JUMP_NE(L_remainder_not_integer);
  MOV(R0, INDD(R0,1));
  MOV(R1, INDD(R1,1));
  REM(R0, R1);
  PUSH(R0);
  CALL(MAKE_SOB_INTEGER);
  DROP(1);
  POP(FP);
  RETURN;

L_remainder_closure:
  PUSH(IMM(3));
  CALL(MALLOC);
  DROP(1);
  MOV(INDD(R0, 0), IMM(T_CLOSURE));
  MOV(INDD(R0, 1), IMM(0));
  MOV(INDD(R0, 2), LABEL(L_remainder_code));
  MOV(IND(34), R0);

// FVAR set-car!
  JUMP(L_set_car_closure);
L_set_car_code:
  PUSH(FP);
  MOV(FP, SP);
  CMP(FPARG(1), IMM(2));
  JUMP_NE(L_closure_error_args_count);
  MOV(R0, FPARG(2));
  CMP(INDD(R0, 0), IMM(T_PAIR));
  JUMP_NE(L_error_set_car_not_pair);
  MOV(R1, FPARG(3));
  MOV(INDD(R0,1),R1);
  MOV(R0, SOB_VOID);
  POP(FP);
  RETURN;

L_set_car_closure:
  PUSH(IMM(3));
  CALL(MALLOC);
  DROP(1);
  MOV(INDD(R0, 0), IMM(T_CLOSURE));
  MOV(INDD(R0, 1), IMM(0));
  MOV(INDD(R0, 2), LABEL(L_set_car_code));
  MOV(IND(35), R0);

// FVAR set-cdr!
  JUMP(L_set_cdr_closure);
L_set_cdr_code:
  PUSH(FP);
  MOV(FP, SP);
  CMP(FPARG(1), IMM(2));
  JUMP_NE(L_closure_error_args_count);
  MOV(R0, FPARG(2));
  CMP(INDD(R0, 0), IMM(T_PAIR));
  JUMP_NE(L_error_set_cdr_not_pair);
  MOV(R1, FPARG(3));
  MOV(INDD(R0,2),R1);
  MOV(R0, SOB_VOID);
  POP(FP);
  RETURN;

L_set_cdr_closure:
  PUSH(IMM(3));
  CALL(MALLOC);
  DROP(1);
  MOV(INDD(R0, 0), IMM(T_CLOSURE));
  MOV(INDD(R0, 1), IMM(0));
  MOV(INDD(R0, 2), LABEL(L_set_cdr_code));
  MOV(IND(36), R0);

// FVAR-string-length
  JUMP(L_string_length_closure);
L_string_length_code:
  PUSH(FP);
  MOV(FP, SP);
  CMP(FPARG(1), IMM(1));
  JUMP_NE(L_closure_error_args_count);
  MOV(R1, FPARG(2));
  CMP(INDD(R1, 0), IMM(T_STRING));
  JUMP_NE(L_error_string_length_not_string);
  PUSH(INDD(R1, 1));
  CALL(MAKE_SOB_INTEGER);
  DROP(1);
  POP(FP);
  RETURN;

L_string_length_closure:
  PUSH(IMM(3));
  CALL(MALLOC);
  DROP(1);
  MOV(INDD(R0, 0), IMM(T_CLOSURE));
  MOV(INDD(R0, 1), IMM(0));
  MOV(INDD(R0, 2), LABEL(L_string_length_code));
MOV(IND(37), R0);

// FVAR-string-ref
  JUMP(L_string_ref_closure);
L_string_ref_code:
  PUSH(FP);
  MOV(FP, SP);
  CMP(FPARG(1), IMM(2));
  JUMP_NE(L_closure_error_args_count);
  MOV(R1, FPARG(2));
  CMP(INDD(R1, 0), IMM(T_STRING));
  JUMP_NE(L_error_string_ref_not_string);
  MOV(R2, FPARG(3));
  CMP(INDD(R2, 0), IMM(T_INTEGER));
  JUMP_NE(L_error_string_ref_not_integer);
  MOV(R3,INDD(R2,1));
  ADD(R3,IMM(2));
  PUSH(INDD(R1, R3));
  CALL(MAKE_SOB_CHAR);
  DROP(1);
  POP(FP);
  RETURN;

L_string_ref_closure:
  PUSH(IMM(3));
  CALL(MALLOC);
  DROP(1);
  MOV(INDD(R0, 0), IMM(T_CLOSURE));
  MOV(INDD(R0, 1), IMM(0));
  MOV(INDD(R0, 2), LABEL(L_string_ref_code));
  MOV(IND(38), R0);

// FVAR string-set!
  JUMP(L_string_set_closure);
L_string_set_code:
  PUSH(FP);
  MOV(FP, SP);
  CMP(FPARG(1), IMM(3));
  JUMP_NE(L_closure_error_args_count);
  MOV(R0,FPARG(2));  CMP(INDD(R0, 0), IMM(T_STRING));
  JUMP_NE(L_string_set_1_not_string);
  MOV(R1,FPARG(3));
  CMP(INDD(R1, 0), IMM(T_INTEGER));
  JUMP_NE(L_string_set_2_not_int);
  MOV(R1,INDD(R1,1));
  ADD(R1,2);
  MOV(R2,FPARG(4));
  CMP(INDD(R2, 0), IMM(T_CHAR));
  JUMP_NE(L_string_set_3_not_char);
  MOV(R2,INDD(R2,1));
  MOV(INDD(R0,R1),R2);
  MOV(R0,SOB_VOID);
  POP(FP);
  RETURN;

L_string_set_closure:
  PUSH(IMM(3));
  CALL(MALLOC);
  DROP(1);
  MOV(INDD(R0, 0), IMM(T_CLOSURE));
  MOV(INDD(R0, 1), IMM(0));
  MOV(INDD(R0, 2), LABEL(L_string_set_code));
  MOV(IND(39), R0);

// FVAR string->symbol
  JUMP(L_string2symbol_closure);
L_string2symbol_code:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R1,FPARG(1));
  CMP(R1,IMM(1));
  JUMP_NE(L_closure_error_args_count);
  MOV(R2,FPARG(2));
  CMP(INDD(R2,0),IMM(T_STRING));
  JUMP_NE(L_string2symbol_not_string);
  MOV(R3,R15);
  CMP(IND(R3),IMM(T_NIL));
  JUMP_EQ(L_empty_symbol_table);
L_string2symbol_strcmp:
  MOV(R5,INDD(R3,1));
  MOV(R13, INDD(R5,1));
  MOV(R9,INDD(R13,1));
  CMP(R9,INDD(R2,1));
  JUMP_NE(L_string2symbol_next);
  MOV(R10,IMM(2));
L_string2symbol_char_cmp:
  CMP(INDD(R2,R10),INDD(R13,R10));
  JUMP_NE(L_string2symbol_next);
  INCR(R10);
  DECR(R9);
  CMP(R9,IMM(0));
  JUMP_EQ(L_string2symbol_found);
  JUMP(L_string2symbol_char_cmp);
L_string2symbol_next:
  CMP(INDD(R3,2),IMM(T_NIL));
  JUMP_EQ(L_string2symbol_not_found);
  MOV(R3,INDD(R3,2));
  JUMP(L_string2symbol_strcmp);
L_string2symbol_found:
  MOV(R0, R5);
  JUMP(L_string2symbol_end);
L_string2symbol_not_found:
  PUSH(R2);
  CALL(MAKE_SOB_SYMBOL);
  DROP(1);
  MOV(R14, R0);
  PUSH(R15);
  PUSH(R0);
  CALL(MAKE_SOB_PAIR);
  DROP(2);
  MOV(R15, R0);
  MOV(R0, R14);
  JUMP(L_string2symbol_end);
L_empty_symbol_table:
  PUSH(R2);
  CALL(MAKE_SOB_SYMBOL);
  DROP(1);
  MOV(R14, R0);
  PUSH(IMM(T_NIL));  PUSH(R0);
  CALL(MAKE_SOB_PAIR);
  DROP(2);
  MOV(R15, R0);
  MOV(R0,R14);
L_string2symbol_end:
  POP(FP);
  RETURN;
L_string2symbol_closure:
  PUSH(IMM(3));
  CALL(MALLOC);
  DROP(1);
  MOV(INDD(R0, 0), IMM(T_CLOSURE));
  MOV(INDD(R0, 1), IMM(0));
  MOV(INDD(R0, 2), LABEL(L_string2symbol_code));
  MOV(IND(40), R0);

// FVAR-string?
  JUMP(L_string_closure);
L_string_code:
  PUSH(FP);
  MOV(FP, SP);
  CMP(FPARG(1), IMM(1));
  JUMP_NE(L_closure_error_args_count);
  MOV(R1, FPARG(2));
  CMP(INDD(R1, 0), IMM(T_STRING));
  JUMP_EQ(L_string_true);
  MOV(R0, IMM(SOB_FALSE));
  JUMP(L_string_end);
L_string_true:
  MOV(R0, IMM(SOB_TRUE));
L_string_end:
  POP(FP);
  RETURN;

L_string_closure:
  PUSH(IMM(3));
  CALL(MALLOC);
  DROP(1);
  MOV(INDD(R0, 0), IMM(T_CLOSURE));
  MOV(INDD(R0, 1), IMM(0));
  MOV(INDD(R0, 2), LABEL(L_string_code));
  MOV(IND(41), R0);

// FVAR-symbol?
  JUMP(L_symbol_closure);
L_symbol_code:
  PUSH(FP);
  MOV(FP, SP);
  CMP(FPARG(1), IMM(1));
  JUMP_NE(L_closure_error_args_count);
  MOV(R1, FPARG(2));
  CMP(INDD(R1, 0), IMM(T_SYMBOL));
  JUMP_NE(L_zero_code_false);
  CMP(INDD(R1, 1), IMM(0));
  JUMP_EQ(L_zero_code_true);
L_symbol_code_false:
  MOV(R0, IMM(SOB_FALSE));
  JUMP(L_zero_end);
L_symbol_code_true:
  MOV(R0, IMM(SOB_TRUE));
L_symbol_end:
  POP(FP);
  RETURN;

L_symbol_closure:
  PUSH(IMM(3));
  CALL(MALLOC);
  DROP(1);
  MOV(INDD(R0, 0), IMM(T_CLOSURE));
  MOV(INDD(R0, 1), IMM(0));
  MOV(INDD(R0, 2), LABEL(L_symbol_code));
MOV(IND(42), R0);

// FVAR symbol->string
  JUMP(L_symbol2string_closure);
L_symbol2string_code:
  PUSH(FP);
  MOV(FP, SP);
  PUSH(R1);
  MOV(R0,FPARG(2));
  MOV(R1,INDD(R0,1));
  MOV(R0,R1);
  POP(R1);
  POP(FP);
  RETURN;
L_symbol2string_closure:
  PUSH(IMM(3));
  CALL(MALLOC);
  DROP(1);
  MOV(INDD(R0, 0), IMM(T_CLOSURE));
  MOV(INDD(R0, 1), IMM(0));
  MOV(INDD(R0, 2), LABEL(L_symbol2string_code));
  MOV(IND(43), R0);

// FVAR vector
  JUMP(L_vector_var_closure);
L_vector_var_code:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R1,FPARG(1));
  MOV(R2,IMM(2));
L_vector_args:
  CMP(R1,IMM(0));
  JUMP_EQ(L_vector_args_end);
  MOV(R3,FPARG(R2));
  PUSH(R3);
  DECR(R1);
  INCR(R2);
  JUMP(L_vector_args);
L_vector_args_end:
  MOV(R4, FPARG(1));
  PUSH(R4);
  CALL(MAKE_SOB_VECTOR);
  POP(R4);
  DROP(R4);
  POP(FP);
  RETURN;
L_vector_var_closure:
  PUSH(IMM(3));
  CALL(MALLOC);
  DROP(1);
  MOV(INDD(R0, 0), IMM(T_CLOSURE));
  MOV(INDD(R0, 1), IMM(0));
  MOV(INDD(R0, 2), LABEL(L_vector_var_code));
  MOV(IND(44), R0);

// FVAR-vector-length
  JUMP(L_vector_length_closure);
L_vector_length_code:
  PUSH(FP);
  MOV(FP, SP);
  CMP(FPARG(1), IMM(1));
  JUMP_NE(L_closure_error_args_count);
  MOV(R1, FPARG(2));
  CMP(INDD(R1, 0), IMM(T_VECTOR));
  JUMP_NE(L_error_vector_length_not_vector);
  PUSH(INDD(R1, 1));
  CALL(MAKE_SOB_INTEGER);
  DROP(1);
  POP(FP);
  RETURN;

L_vector_length_closure:
  PUSH(IMM(3));
  CALL(MALLOC);
  DROP(1);
  MOV(INDD(R0, 0), IMM(T_CLOSURE));
  MOV(INDD(R0, 1), IMM(0));
  MOV(INDD(R0, 2), LABEL(L_vector_length_code));
MOV(IND(45), R0);

// FVAR-vector-ref
  JUMP(L_vector_ref_closure);
L_vector_ref_code:
  PUSH(FP);
  MOV(FP, SP);
  CMP(FPARG(1), IMM(2));
  JUMP_NE(L_closure_error_args_count);
  MOV(R1, FPARG(2));
  CMP(INDD(R1, 0), IMM(T_VECTOR));
  JUMP_NE(L_error_vector_ref_not_vector);
  MOV(R2, FPARG(3));
  CMP(INDD(R2, 0), IMM(T_INTEGER));
  JUMP_NE(L_error_vector_ref_not_integer);
  MOV(R3,INDD(R2,1));
  ADD(R3,IMM(2));
  MOV(R0, INDD(R1,R3));
  POP(FP);
  RETURN;

L_vector_ref_closure:
  PUSH(IMM(3));
  CALL(MALLOC);
  DROP(1);
  MOV(INDD(R0, 0), IMM(T_CLOSURE));
  MOV(INDD(R0, 1), IMM(0));
  MOV(INDD(R0, 2), LABEL(L_vector_ref_code));
  MOV(IND(46), R0);

// FVAR vector-set!
  JUMP(L_vector_set_closure);
L_vector_set_code:
  PUSH(FP);
  MOV(FP, SP);
  CMP(FPARG(1), IMM(3));
  JUMP_NE(L_closure_error_args_count);
  MOV(R0,FPARG(2));  CMP(INDD(R0, 0), IMM(T_VECTOR));
  JUMP_NE(L_vector_set_1_not_vector);
  MOV(R1,FPARG(3));
  CMP(INDD(R1, 0), IMM(T_INTEGER));
  JUMP_NE(L_vector_set_2_not_int);
  MOV(R1,INDD(R1,1));
  ADD(R1,2);
  MOV(R2,FPARG(4));
  MOV(INDD(R0,R1),R2);
  MOV(R0,SOB_VOID);
  POP(FP);
  RETURN;

L_vector_set_closure:
  PUSH(IMM(3));
  CALL(MALLOC);
  DROP(1);
  MOV(INDD(R0, 0), IMM(T_CLOSURE));
  MOV(INDD(R0, 1), IMM(0));
  MOV(INDD(R0, 2), LABEL(L_vector_set_code));
  MOV(IND(47), R0);

// FVAR-vector?
  JUMP(L_vector_closure);
L_vector_code:
  PUSH(FP);
  MOV(FP, SP);
  CMP(FPARG(1), IMM(1));
  JUMP_NE(L_closure_error_args_count);
  MOV(R1, FPARG(2));
  CMP(INDD(R1, 0), IMM(T_VECTOR));
  JUMP_EQ(L_vector_true);
  MOV(R0, IMM(SOB_FALSE));
  JUMP(L_vector_end);
L_vector_true:
  MOV(R0, IMM(SOB_TRUE));
L_vector_end:
  POP(FP);
  RETURN;

L_vector_closure:
  PUSH(IMM(3));
  CALL(MALLOC);
  DROP(1);
  MOV(INDD(R0, 0), IMM(T_CLOSURE));
  MOV(INDD(R0, 1), IMM(0));
  MOV(INDD(R0, 2), LABEL(L_vector_code));
  MOV(IND(48), R0);

// FVAR-zero?
  JUMP(L_zero_closure);
L_zero_code:
  PUSH(FP);
  MOV(FP, SP);
  CMP(FPARG(1), IMM(1));
  JUMP_NE(L_closure_error_args_count);
  MOV(R1, FPARG(2));
  CMP(INDD(R1, 0), IMM(T_INTEGER));
  JUMP_NE(L_zero_code_false);
  CMP(INDD(R1, 1), IMM(0));
  JUMP_EQ(L_zero_code_true);
L_zero_code_false:
  MOV(R0, IMM(SOB_FALSE));
  JUMP(L_zero_end);
L_zero_code_true:
  MOV(R0, IMM(SOB_TRUE));
L_zero_end:
  POP(FP);
  RETURN;

L_zero_closure:
  PUSH(IMM(3));
  CALL(MALLOC);
  DROP(1);
  MOV(INDD(R0, 0), IMM(T_CLOSURE));
  MOV(INDD(R0, 1), IMM(0));
  MOV(INDD(R0, 2), LABEL(L_zero_code));
MOV(IND(49), R0);

// *********************************
//          FVAR CODE  END
// *********************************

// *********************************
//          SYMBOL TABLE 
// *********************************

  MOV(R15,IND(0));
  CALL(MAKE_SOB_NIL);
  MOV(IND(R15),IND(R0));
// *********************************
//          SYMBOL TABLE END
// *********************************

// ***CODE-GEN DEF***

// ***CODE-GEN LAMBDA-VAR***
  MOV(R1, FPARG(0));
  MOV(R3,IMM(1));
  PUSH(R3);
  CALL(MALLOC);
  DROP(1);
  MOV(R2, R0);
// Copy environments
  MOV(R3, IMM(0));
  MOV(R4, IMM(1));
L_closure_lambda_var_loop_copy_3:
  CMP(R3, IMM(0));
  JUMP_EQ (L_closure_lambda_var_loop_copy_end_3);
  MOV(INDD(R2, R4), INDD(R1, R3));
  INCR(R3);
  INCR(R4);
  JUMP(L_closure_lambda_var_loop_copy_3);
L_closure_lambda_var_loop_copy_end_3:
  CMP(IMM(0), IMM(0));
  JUMP_EQ(L_closure_lambda_var_loop_copy_stack_end_3)
  MOV(R3,FPARG(1));
  PUSH(R3);
  CALL(MALLOC);
  DROP(1);
  MOV(INDD(R2,0), R0);
// Copy from stack
  MOV(R4, IMM(0));
  MOV(R5, IMM(2));
L_closure_lambda_var_loop_copy_stack_3:
  CMP(R4, R3);
  JUMP_EQ(L_closure_lambda_var_loop_copy_stack_end_3);
  MOV(INDD(INDD(R2,0),R4),FPARG(R5));
  INCR(R4);
  INCR(R5);
  JUMP(L_closure_lambda_var_loop_copy_stack_3);
L_closure_lambda_var_loop_copy_stack_end_3:
// Create Closure
  PUSH(IMM(3));
  CALL(MALLOC);
  DROP(1);
  MOV(INDD(R0,0),IMM(T_CLOSURE));
  MOV(INDD(R0,1),R2);
  MOV(INDD(R0,2),LABEL(L_closure_lambda_var_body_3));
  JUMP(L_closure_lambda_var_end_3);

// Closure body
L_closure_lambda_var_body_3:
  PUSH(FP);
  MOV(FP,SP);
//Converting variadic Parameters to List
  MOV(R1, SOB_NIL);
  MOV(R4, FPARG(1));
  INCR(R4);
L_closure_lambda_var_loop_create_list_3:
  CMP(R4, IMM(1));
  JUMP_EQ(L_closure_lambda_var_loop_create_list_end_3);
  PUSH(R1);
  PUSH(FPARG(R4));
  CALL(MAKE_SOB_PAIR);
  DROP(2);
  MOV(R1, R0);
  DECR(R4);
  JUMP(L_closure_lambda_var_loop_create_list_3);
L_closure_lambda_var_loop_create_list_end_3:
  MOV(FPARG(2), R1);
// CODE-GEN pvar
  MOV(R0, FPARG(2));

  POP(FP);
  RETURN;
L_closure_lambda_var_end_3:

  MOV(IND(50), R0);
  MOV(R0, IMM(SOB_VOID));
// ***CODE-GEN DEF***

// ***CODE-GEN LAMBDA-SIMPLE***
  MOV(R1, FPARG(0));
  MOV(R3,IMM(1));
  PUSH(R3);
  CALL(MALLOC);
  DROP(1);
  MOV(R2,R0);
// Copy environments
  MOV(R3, IMM(0));
  MOV(R4, IMM(1));
L_closure_loop_copy_env_lambda_simple_11:
  CMP(R3, IMM(0));
  JUMP_EQ (L_closure_loop_copy_env_end_lambda_simple_11);
  MOV(INDD(R2, R4), INDD(R1, R3));
  INCR(R3);
  INCR(R4);
  JUMP(L_closure_loop_copy_env_lambda_simple_11);
L_closure_loop_copy_env_end_lambda_simple_11:
  CMP(IMM(0), IMM(0));
  JUMP_EQ(L_closure_loop_copy_stack_end_lambda_simple_11)
  MOV(R3,FPARG(1));
  PUSH(R3);
  CALL(MALLOC);
  DROP(1);
  MOV(INDD(R2,0), R0);
// Copy from stack
  MOV(R4, IMM(0));
  MOV(R5, IMM(2));
L_closure_loop_copy_stack_lambda_simple_11:
  CMP(R4, R3);
  JUMP_EQ(L_closure_loop_copy_stack_end_lambda_simple_11);
  MOV(INDD(INDD(R2,0),R4),FPARG(R5));
  INCR(R4);
  INCR(R5);
  JUMP(L_closure_loop_copy_stack_lambda_simple_11);
L_closure_loop_copy_stack_end_lambda_simple_11:
// Create Closure
  PUSH(IMM(3));
  CALL(MALLOC);
  DROP(1);
  MOV(INDD(R0,0),IMM(T_CLOSURE));
  MOV(INDD(R0,1),R2);
  MOV(INDD(R0,2),LABEL(L_closure_body_lambda_simple_11));
  JUMP(L_closure_exit_lambda_simple_11);

// Closure body
L_closure_body_lambda_simple_11:
  PUSH(FP);
  MOV(FP,SP);
  CMP(FPARG(1), IMM(2));
  JUMP_NE(L_closure_error_args_count);

// ***CODE-GEN IF***

// ***CODE-GEN APPLIC***
// Push Parameters
// CODE-GEN pvar
  MOV(R0, FPARG(3));

  PUSH(R0);
// Push Parameters Number
  MOV(R0, IMM(1));
  PUSH(R0);
// Push Closure
// CODE-GEN fvar
  MOV(R0, IND(28));

  CMP(INDD(R0, 0), IMM(T_CLOSURE));
  JUMP_NE(L_error_cannot_apply_none_closure);
// Push Environment
  PUSH(INDD(R0, 1));
// APPLIC
  CALLA(INDD(R0, 2));
  DROP(1);
  POP(R1);
  DROP(R1);

  CMP(R0, IMM(SOB_FALSE));
  JUMP_EQ(L_if3_else_8);
// CODE-GEN pvar
  MOV(R0, FPARG(3));

  JUMP(L_if3_exit_8);
L_if3_else_8:
// ***CODE-GEN TC-APPLIC***
  PUSH(SOB_NIL);

// ***CODE-GEN APPLIC***
// Push Parameters

// ***CODE-GEN APPLIC***
// Push Parameters
// CODE-GEN pvar
  MOV(R0, FPARG(3));

  PUSH(R0);
// Push Parameters Number
  MOV(R0, IMM(1));
  PUSH(R0);
// Push Closure
// CODE-GEN fvar
  MOV(R0, IND(17));

  CMP(INDD(R0, 0), IMM(T_CLOSURE));
  JUMP_NE(L_error_cannot_apply_none_closure);
// Push Environment
  PUSH(INDD(R0, 1));
// APPLIC
  CALLA(INDD(R0, 2));
  DROP(1);
  POP(R1);
  DROP(R1);

  PUSH(R0);
// CODE-GEN pvar
  MOV(R0, FPARG(2));

  PUSH(R0);
// Push Parameters Number
  MOV(R0, IMM(2));
  PUSH(R0);
// Push Closure
// CODE-GEN fvar
  MOV(R0, IND(51));

  CMP(INDD(R0, 0), IMM(T_CLOSURE));
  JUMP_NE(L_error_cannot_apply_none_closure);
// Push Environment
  PUSH(INDD(R0, 1));
// APPLIC
  CALLA(INDD(R0, 2));
  DROP(1);
  POP(R1);
  DROP(R1);

  PUSH(R0);

// ***CODE-GEN APPLIC***
// Push Parameters

// ***CODE-GEN APPLIC***
// Push Parameters
// CODE-GEN pvar
  MOV(R0, FPARG(3));

  PUSH(R0);
// Push Parameters Number
  MOV(R0, IMM(1));
  PUSH(R0);
// Push Closure
// CODE-GEN fvar
  MOV(R0, IND(16));

  CMP(INDD(R0, 0), IMM(T_CLOSURE));
  JUMP_NE(L_error_cannot_apply_none_closure);
// Push Environment
  PUSH(INDD(R0, 1));
// APPLIC
  CALLA(INDD(R0, 2));
  DROP(1);
  POP(R1);
  DROP(R1);

  PUSH(R0);
// Push Parameters Number
  MOV(R0, IMM(1));
  PUSH(R0);
// Push Closure
// CODE-GEN pvar
  MOV(R0, FPARG(2));

  CMP(INDD(R0, 0), IMM(T_CLOSURE));
  JUMP_NE(L_error_cannot_apply_none_closure);
// Push Environment
  PUSH(INDD(R0, 1));
// APPLIC
  CALLA(INDD(R0, 2));
  DROP(1);
  POP(R1);
  DROP(R1);

  PUSH(R0);

  MOV(R0, IMM(2));
  PUSH(R0);
// CODE-GEN fvar
  MOV(R0, IND(20));

  CMP(INDD(R0,0), IMM(T_CLOSURE));
  JUMP_NE(L_error_cannot_apply_none_closure);
  PUSH(INDD(R0,1));
  PUSH(FPARG(-1));
  MOV(R1,FP);
  MOV(R2,FPARG(1));
  ADD(R2,IMM(5));
  MOV(R3,FP);
  SUB(R3,R2);
  MOV(FP,FPARG(-2));
  MOV(R4,SP);
  DECR(R4);
L_tc_applic_move_stack_10:
  MOV(STACK(R3),STACK(R1));
  INCR(R1);
  INCR(R3);
  CMP(R1,R4);
  JUMP_LE(L_tc_applic_move_stack_10);
  MOV(SP,R3);
  JUMPA(INDD(R0,2));

L_if3_exit_8:

  POP(FP);
  RETURN;
L_closure_exit_lambda_simple_11:

  MOV(IND(51), R0);
  MOV(R0, IMM(SOB_VOID));
// ***CODE-GEN DEF***

// ***CODE-GEN LAMBDA-SIMPLE***
  MOV(R1, FPARG(0));
  MOV(R3,IMM(1));
  PUSH(R3);
  CALL(MALLOC);
  DROP(1);
  MOV(R2,R0);
// Copy environments
  MOV(R3, IMM(0));
  MOV(R4, IMM(1));
L_closure_loop_copy_env_lambda_simple_12:
  CMP(R3, IMM(0));
  JUMP_EQ (L_closure_loop_copy_env_end_lambda_simple_12);
  MOV(INDD(R2, R4), INDD(R1, R3));
  INCR(R3);
  INCR(R4);
  JUMP(L_closure_loop_copy_env_lambda_simple_12);
L_closure_loop_copy_env_end_lambda_simple_12:
  CMP(IMM(0), IMM(0));
  JUMP_EQ(L_closure_loop_copy_stack_end_lambda_simple_12)
  MOV(R3,FPARG(1));
  PUSH(R3);
  CALL(MALLOC);
  DROP(1);
  MOV(INDD(R2,0), R0);
// Copy from stack
  MOV(R4, IMM(0));
  MOV(R5, IMM(2));
L_closure_loop_copy_stack_lambda_simple_12:
  CMP(R4, R3);
  JUMP_EQ(L_closure_loop_copy_stack_end_lambda_simple_12);
  MOV(INDD(INDD(R2,0),R4),FPARG(R5));
  INCR(R4);
  INCR(R5);
  JUMP(L_closure_loop_copy_stack_lambda_simple_12);
L_closure_loop_copy_stack_end_lambda_simple_12:
// Create Closure
  PUSH(IMM(3));
  CALL(MALLOC);
  DROP(1);
  MOV(INDD(R0,0),IMM(T_CLOSURE));
  MOV(INDD(R0,1),R2);
  MOV(INDD(R0,2),LABEL(L_closure_body_lambda_simple_12));
  JUMP(L_closure_exit_lambda_simple_12);

// Closure body
L_closure_body_lambda_simple_12:
  PUSH(FP);
  MOV(FP,SP);
  CMP(FPARG(1), IMM(2));
  JUMP_NE(L_closure_error_args_count);

// ***CODE-GEN IF***

// ***CODE-GEN APPLIC***
// Push Parameters
// CODE-GEN pvar
  MOV(R0, FPARG(2));

  PUSH(R0);
// Push Parameters Number
  MOV(R0, IMM(1));
  PUSH(R0);
// Push Closure
// CODE-GEN fvar
  MOV(R0, IND(28));

  CMP(INDD(R0, 0), IMM(T_CLOSURE));
  JUMP_NE(L_error_cannot_apply_none_closure);
// Push Environment
  PUSH(INDD(R0, 1));
// APPLIC
  CALLA(INDD(R0, 2));
  DROP(1);
  POP(R1);
  DROP(R1);

  CMP(R0, IMM(SOB_FALSE));
  JUMP_EQ(L_if3_else_9);
// CODE-GEN pvar
  MOV(R0, FPARG(3));

  JUMP(L_if3_exit_9);
L_if3_else_9:
// ***CODE-GEN TC-APPLIC***
  PUSH(SOB_NIL);

// ***CODE-GEN APPLIC***
// Push Parameters
// CODE-GEN pvar
  MOV(R0, FPARG(3));

  PUSH(R0);

// ***CODE-GEN APPLIC***
// Push Parameters
// CODE-GEN pvar
  MOV(R0, FPARG(2));

  PUSH(R0);
// Push Parameters Number
  MOV(R0, IMM(1));
  PUSH(R0);
// Push Closure
// CODE-GEN fvar
  MOV(R0, IND(17));

  CMP(INDD(R0, 0), IMM(T_CLOSURE));
  JUMP_NE(L_error_cannot_apply_none_closure);
// Push Environment
  PUSH(INDD(R0, 1));
// APPLIC
  CALLA(INDD(R0, 2));
  DROP(1);
  POP(R1);
  DROP(R1);

  PUSH(R0);
// Push Parameters Number
  MOV(R0, IMM(2));
  PUSH(R0);
// Push Closure
// CODE-GEN fvar
  MOV(R0, IND(52));

  CMP(INDD(R0, 0), IMM(T_CLOSURE));
  JUMP_NE(L_error_cannot_apply_none_closure);
// Push Environment
  PUSH(INDD(R0, 1));
// APPLIC
  CALLA(INDD(R0, 2));
  DROP(1);
  POP(R1);
  DROP(R1);

  PUSH(R0);

// ***CODE-GEN APPLIC***
// Push Parameters
// CODE-GEN pvar
  MOV(R0, FPARG(2));

  PUSH(R0);
// Push Parameters Number
  MOV(R0, IMM(1));
  PUSH(R0);
// Push Closure
// CODE-GEN fvar
  MOV(R0, IND(16));

  CMP(INDD(R0, 0), IMM(T_CLOSURE));
  JUMP_NE(L_error_cannot_apply_none_closure);
// Push Environment
  PUSH(INDD(R0, 1));
// APPLIC
  CALLA(INDD(R0, 2));
  DROP(1);
  POP(R1);
  DROP(R1);

  PUSH(R0);

  MOV(R0, IMM(2));
  PUSH(R0);
// CODE-GEN fvar
  MOV(R0, IND(20));

  CMP(INDD(R0,0), IMM(T_CLOSURE));
  JUMP_NE(L_error_cannot_apply_none_closure);
  PUSH(INDD(R0,1));
  PUSH(FPARG(-1));
  MOV(R1,FP);
  MOV(R2,FPARG(1));
  ADD(R2,IMM(5));
  MOV(R3,FP);
  SUB(R3,R2);
  MOV(FP,FPARG(-2));
  MOV(R4,SP);
  DECR(R4);
L_tc_applic_move_stack_11:
  MOV(STACK(R3),STACK(R1));
  INCR(R1);
  INCR(R3);
  CMP(R1,R4);
  JUMP_LE(L_tc_applic_move_stack_11);
  MOV(SP,R3);
  JUMPA(INDD(R0,2));

L_if3_exit_9:

  POP(FP);
  RETURN;
L_closure_exit_lambda_simple_12:

  MOV(IND(52), R0);
  MOV(R0, IMM(SOB_VOID));

// ***CODE-GEN APPLIC***
// Push Parameters

// ***CODE-GEN LAMBDA-SIMPLE***
  MOV(R1, FPARG(0));
  MOV(R3,IMM(1));
  PUSH(R3);
  CALL(MALLOC);
  DROP(1);
  MOV(R2,R0);
// Copy environments
  MOV(R3, IMM(0));
  MOV(R4, IMM(1));
L_closure_loop_copy_env_lambda_simple_19:
  CMP(R3, IMM(0));
  JUMP_EQ (L_closure_loop_copy_env_end_lambda_simple_19);
  MOV(INDD(R2, R4), INDD(R1, R3));
  INCR(R3);
  INCR(R4);
  JUMP(L_closure_loop_copy_env_lambda_simple_19);
L_closure_loop_copy_env_end_lambda_simple_19:
  CMP(IMM(0), IMM(0));
  JUMP_EQ(L_closure_loop_copy_stack_end_lambda_simple_19)
  MOV(R3,FPARG(1));
  PUSH(R3);
  CALL(MALLOC);
  DROP(1);
  MOV(INDD(R2,0), R0);
// Copy from stack
  MOV(R4, IMM(0));
  MOV(R5, IMM(2));
L_closure_loop_copy_stack_lambda_simple_19:
  CMP(R4, R3);
  JUMP_EQ(L_closure_loop_copy_stack_end_lambda_simple_19);
  MOV(INDD(INDD(R2,0),R4),FPARG(R5));
  INCR(R4);
  INCR(R5);
  JUMP(L_closure_loop_copy_stack_lambda_simple_19);
L_closure_loop_copy_stack_end_lambda_simple_19:
// Create Closure
  PUSH(IMM(3));
  CALL(MALLOC);
  DROP(1);
  MOV(INDD(R0,0),IMM(T_CLOSURE));
  MOV(INDD(R0,1),R2);
  MOV(INDD(R0,2),LABEL(L_closure_body_lambda_simple_19));
  JUMP(L_closure_exit_lambda_simple_19);

// Closure body
L_closure_body_lambda_simple_19:
  PUSH(FP);
  MOV(FP,SP);
  CMP(FPARG(1), IMM(2));
  JUMP_NE(L_closure_error_args_count);
// CODE-GEN pvar
  MOV(R0, FPARG(2));

  POP(FP);
  RETURN;
L_closure_exit_lambda_simple_19:

  PUSH(R0);
// Push Parameters Number
  MOV(R0, IMM(1));
  PUSH(R0);
// Push Closure

// ***CODE-GEN APPLIC***
// Push Parameters

// ***CODE-GEN LAMBDA-SIMPLE***
  MOV(R1, FPARG(0));
  MOV(R3,IMM(1));
  PUSH(R3);
  CALL(MALLOC);
  DROP(1);
  MOV(R2,R0);
// Copy environments
  MOV(R3, IMM(0));
  MOV(R4, IMM(1));
L_closure_loop_copy_env_lambda_simple_18:
  CMP(R3, IMM(0));
  JUMP_EQ (L_closure_loop_copy_env_end_lambda_simple_18);
  MOV(INDD(R2, R4), INDD(R1, R3));
  INCR(R3);
  INCR(R4);
  JUMP(L_closure_loop_copy_env_lambda_simple_18);
L_closure_loop_copy_env_end_lambda_simple_18:
  CMP(IMM(0), IMM(0));
  JUMP_EQ(L_closure_loop_copy_stack_end_lambda_simple_18)
  MOV(R3,FPARG(1));
  PUSH(R3);
  CALL(MALLOC);
  DROP(1);
  MOV(INDD(R2,0), R0);
// Copy from stack
  MOV(R4, IMM(0));
  MOV(R5, IMM(2));
L_closure_loop_copy_stack_lambda_simple_18:
  CMP(R4, R3);
  JUMP_EQ(L_closure_loop_copy_stack_end_lambda_simple_18);
  MOV(INDD(INDD(R2,0),R4),FPARG(R5));
  INCR(R4);
  INCR(R5);
  JUMP(L_closure_loop_copy_stack_lambda_simple_18);
L_closure_loop_copy_stack_end_lambda_simple_18:
// Create Closure
  PUSH(IMM(3));
  CALL(MALLOC);
  DROP(1);
  MOV(INDD(R0,0),IMM(T_CLOSURE));
  MOV(INDD(R0,1),R2);
  MOV(INDD(R0,2),LABEL(L_closure_body_lambda_simple_18));
  JUMP(L_closure_exit_lambda_simple_18);

// Closure body
L_closure_body_lambda_simple_18:
  PUSH(FP);
  MOV(FP,SP);
  CMP(FPARG(1), IMM(1));
  JUMP_NE(L_closure_error_args_count);
// ***CODE-GEN TC-APPLIC***
  PUSH(SOB_NIL);
  MOV(R0, IMM(5));

  PUSH(R0);
  MOV(R0, IMM(3));

  PUSH(R0);

  MOV(R0, IMM(2));
  PUSH(R0);
// CODE-GEN pvar
  MOV(R0, FPARG(2));

  CMP(INDD(R0,0), IMM(T_CLOSURE));
  JUMP_NE(L_error_cannot_apply_none_closure);
  PUSH(INDD(R0,1));
  PUSH(FPARG(-1));
  MOV(R1,FP);
  MOV(R2,FPARG(1));
  ADD(R2,IMM(5));
  MOV(R3,FP);
  SUB(R3,R2);
  MOV(FP,FPARG(-2));
  MOV(R4,SP);
  DECR(R4);
L_tc_applic_move_stack_15:
  MOV(STACK(R3),STACK(R1));
  INCR(R1);
  INCR(R3);
  CMP(R1,R4);
  JUMP_LE(L_tc_applic_move_stack_15);
  MOV(SP,R3);
  JUMPA(INDD(R0,2));

  POP(FP);
  RETURN;
L_closure_exit_lambda_simple_18:

  PUSH(R0);
// Push Parameters Number
  MOV(R0, IMM(1));
  PUSH(R0);
// Push Closure

// ***CODE-GEN APPLIC***
// Push Parameters

// ***CODE-GEN LAMBDA-SIMPLE***
  MOV(R1, FPARG(0));
  MOV(R3,IMM(1));
  PUSH(R3);
  CALL(MALLOC);
  DROP(1);
  MOV(R2,R0);
// Copy environments
  MOV(R3, IMM(0));
  MOV(R4, IMM(1));
L_closure_loop_copy_env_lambda_simple_15:
  CMP(R3, IMM(0));
  JUMP_EQ (L_closure_loop_copy_env_end_lambda_simple_15);
  MOV(INDD(R2, R4), INDD(R1, R3));
  INCR(R3);
  INCR(R4);
  JUMP(L_closure_loop_copy_env_lambda_simple_15);
L_closure_loop_copy_env_end_lambda_simple_15:
  CMP(IMM(0), IMM(0));
  JUMP_EQ(L_closure_loop_copy_stack_end_lambda_simple_15)
  MOV(R3,FPARG(1));
  PUSH(R3);
  CALL(MALLOC);
  DROP(1);
  MOV(INDD(R2,0), R0);
// Copy from stack
  MOV(R4, IMM(0));
  MOV(R5, IMM(2));
L_closure_loop_copy_stack_lambda_simple_15:
  CMP(R4, R3);
  JUMP_EQ(L_closure_loop_copy_stack_end_lambda_simple_15);
  MOV(INDD(INDD(R2,0),R4),FPARG(R5));
  INCR(R4);
  INCR(R5);
  JUMP(L_closure_loop_copy_stack_lambda_simple_15);
L_closure_loop_copy_stack_end_lambda_simple_15:
// Create Closure
  PUSH(IMM(3));
  CALL(MALLOC);
  DROP(1);
  MOV(INDD(R0,0),IMM(T_CLOSURE));
  MOV(INDD(R0,1),R2);
  MOV(INDD(R0,2),LABEL(L_closure_body_lambda_simple_15));
  JUMP(L_closure_exit_lambda_simple_15);

// Closure body
L_closure_body_lambda_simple_15:
  PUSH(FP);
  MOV(FP,SP);
  CMP(FPARG(1), IMM(1));
  JUMP_NE(L_closure_error_args_count);
// ***CODE-GEN TC-APPLIC***
  PUSH(SOB_NIL);

// ***CODE-GEN LAMBDA-SIMPLE***
  MOV(R1, FPARG(0));
  MOV(R3,IMM(2));
  PUSH(R3);
  CALL(MALLOC);
  DROP(1);
  MOV(R2,R0);
// Copy environments
  MOV(R3, IMM(0));
  MOV(R4, IMM(1));
L_closure_loop_copy_env_lambda_simple_16:
  CMP(R3, IMM(1));
  JUMP_EQ (L_closure_loop_copy_env_end_lambda_simple_16);
  MOV(INDD(R2, R4), INDD(R1, R3));
  INCR(R3);
  INCR(R4);
  JUMP(L_closure_loop_copy_env_lambda_simple_16);
L_closure_loop_copy_env_end_lambda_simple_16:
  CMP(IMM(1), IMM(0));
  JUMP_EQ(L_closure_loop_copy_stack_end_lambda_simple_16)
  MOV(R3,FPARG(1));
  PUSH(R3);
  CALL(MALLOC);
  DROP(1);
  MOV(INDD(R2,0), R0);
// Copy from stack
  MOV(R4, IMM(0));
  MOV(R5, IMM(2));
L_closure_loop_copy_stack_lambda_simple_16:
  CMP(R4, R3);
  JUMP_EQ(L_closure_loop_copy_stack_end_lambda_simple_16);
  MOV(INDD(INDD(R2,0),R4),FPARG(R5));
  INCR(R4);
  INCR(R5);
  JUMP(L_closure_loop_copy_stack_lambda_simple_16);
L_closure_loop_copy_stack_end_lambda_simple_16:
// Create Closure
  PUSH(IMM(3));
  CALL(MALLOC);
  DROP(1);
  MOV(INDD(R0,0),IMM(T_CLOSURE));
  MOV(INDD(R0,1),R2);
  MOV(INDD(R0,2),LABEL(L_closure_body_lambda_simple_16));
  JUMP(L_closure_exit_lambda_simple_16);

// Closure body
L_closure_body_lambda_simple_16:
  PUSH(FP);
  MOV(FP,SP);
  CMP(FPARG(1), IMM(2));
  JUMP_NE(L_closure_error_args_count);

// ***CODE-GEN LAMBDA-SIMPLE***
  MOV(R1, FPARG(0));
  MOV(R3,IMM(3));
  PUSH(R3);
  CALL(MALLOC);
  DROP(1);
  MOV(R2,R0);
// Copy environments
  MOV(R3, IMM(0));
  MOV(R4, IMM(1));
L_closure_loop_copy_env_lambda_simple_17:
  CMP(R3, IMM(2));
  JUMP_EQ (L_closure_loop_copy_env_end_lambda_simple_17);
  MOV(INDD(R2, R4), INDD(R1, R3));
  INCR(R3);
  INCR(R4);
  JUMP(L_closure_loop_copy_env_lambda_simple_17);
L_closure_loop_copy_env_end_lambda_simple_17:
  CMP(IMM(2), IMM(0));
  JUMP_EQ(L_closure_loop_copy_stack_end_lambda_simple_17)
  MOV(R3,FPARG(1));
  PUSH(R3);
  CALL(MALLOC);
  DROP(1);
  MOV(INDD(R2,0), R0);
// Copy from stack
  MOV(R4, IMM(0));
  MOV(R5, IMM(2));
L_closure_loop_copy_stack_lambda_simple_17:
  CMP(R4, R3);
  JUMP_EQ(L_closure_loop_copy_stack_end_lambda_simple_17);
  MOV(INDD(INDD(R2,0),R4),FPARG(R5));
  INCR(R4);
  INCR(R5);
  JUMP(L_closure_loop_copy_stack_lambda_simple_17);
L_closure_loop_copy_stack_end_lambda_simple_17:
// Create Closure
  PUSH(IMM(3));
  CALL(MALLOC);
  DROP(1);
  MOV(INDD(R0,0),IMM(T_CLOSURE));
  MOV(INDD(R0,1),R2);
  MOV(INDD(R0,2),LABEL(L_closure_body_lambda_simple_17));
  JUMP(L_closure_exit_lambda_simple_17);

// Closure body
L_closure_body_lambda_simple_17:
  PUSH(FP);
  MOV(FP,SP);
  CMP(FPARG(1), IMM(1));
  JUMP_NE(L_closure_error_args_count);
// ***CODE-GEN TC-APPLIC***
  PUSH(SOB_NIL);
// CODE-GEN bvar
  MOV(R1, FPARG(0));
  MOV(R1, INDD(R1, 0));
  MOV(R1, INDD(R1, 0));
  MOV(R0, R1);

  PUSH(R0);
// CODE-GEN bvar
  MOV(R1, FPARG(0));
  MOV(R1, INDD(R1, 0));
  MOV(R1, INDD(R1, 1));
  MOV(R0, R1);

  PUSH(R0);

  MOV(R0, IMM(2));
  PUSH(R0);
// CODE-GEN pvar
  MOV(R0, FPARG(2));

  CMP(INDD(R0,0), IMM(T_CLOSURE));
  JUMP_NE(L_error_cannot_apply_none_closure);
  PUSH(INDD(R0,1));
  PUSH(FPARG(-1));
  MOV(R1,FP);
  MOV(R2,FPARG(1));
  ADD(R2,IMM(5));
  MOV(R3,FP);
  SUB(R3,R2);
  MOV(FP,FPARG(-2));
  MOV(R4,SP);
  DECR(R4);
L_tc_applic_move_stack_14:
  MOV(STACK(R3),STACK(R1));
  INCR(R1);
  INCR(R3);
  CMP(R1,R4);
  JUMP_LE(L_tc_applic_move_stack_14);
  MOV(SP,R3);
  JUMPA(INDD(R0,2));

  POP(FP);
  RETURN;
L_closure_exit_lambda_simple_17:

  POP(FP);
  RETURN;
L_closure_exit_lambda_simple_16:

  PUSH(R0);

  MOV(R0, IMM(1));
  PUSH(R0);
// CODE-GEN pvar
  MOV(R0, FPARG(2));

  CMP(INDD(R0,0), IMM(T_CLOSURE));
  JUMP_NE(L_error_cannot_apply_none_closure);
  PUSH(INDD(R0,1));
  PUSH(FPARG(-1));
  MOV(R1,FP);
  MOV(R2,FPARG(1));
  ADD(R2,IMM(5));
  MOV(R3,FP);
  SUB(R3,R2);
  MOV(FP,FPARG(-2));
  MOV(R4,SP);
  DECR(R4);
L_tc_applic_move_stack_13:
  MOV(STACK(R3),STACK(R1));
  INCR(R1);
  INCR(R3);
  CMP(R1,R4);
  JUMP_LE(L_tc_applic_move_stack_13);
  MOV(SP,R3);
  JUMPA(INDD(R0,2));

  POP(FP);
  RETURN;
L_closure_exit_lambda_simple_15:

  PUSH(R0);
// Push Parameters Number
  MOV(R0, IMM(1));
  PUSH(R0);
// Push Closure

// ***CODE-GEN LAMBDA-SIMPLE***
  MOV(R1, FPARG(0));
  MOV(R3,IMM(1));
  PUSH(R3);
  CALL(MALLOC);
  DROP(1);
  MOV(R2,R0);
// Copy environments
  MOV(R3, IMM(0));
  MOV(R4, IMM(1));
L_closure_loop_copy_env_lambda_simple_13:
  CMP(R3, IMM(0));
  JUMP_EQ (L_closure_loop_copy_env_end_lambda_simple_13);
  MOV(INDD(R2, R4), INDD(R1, R3));
  INCR(R3);
  INCR(R4);
  JUMP(L_closure_loop_copy_env_lambda_simple_13);
L_closure_loop_copy_env_end_lambda_simple_13:
  CMP(IMM(0), IMM(0));
  JUMP_EQ(L_closure_loop_copy_stack_end_lambda_simple_13)
  MOV(R3,FPARG(1));
  PUSH(R3);
  CALL(MALLOC);
  DROP(1);
  MOV(INDD(R2,0), R0);
// Copy from stack
  MOV(R4, IMM(0));
  MOV(R5, IMM(2));
L_closure_loop_copy_stack_lambda_simple_13:
  CMP(R4, R3);
  JUMP_EQ(L_closure_loop_copy_stack_end_lambda_simple_13);
  MOV(INDD(INDD(R2,0),R4),FPARG(R5));
  INCR(R4);
  INCR(R5);
  JUMP(L_closure_loop_copy_stack_lambda_simple_13);
L_closure_loop_copy_stack_end_lambda_simple_13:
// Create Closure
  PUSH(IMM(3));
  CALL(MALLOC);
  DROP(1);
  MOV(INDD(R0,0),IMM(T_CLOSURE));
  MOV(INDD(R0,1),R2);
  MOV(INDD(R0,2),LABEL(L_closure_body_lambda_simple_13));
  JUMP(L_closure_exit_lambda_simple_13);

// Closure body
L_closure_body_lambda_simple_13:
  PUSH(FP);
  MOV(FP,SP);
  CMP(FPARG(1), IMM(1));
  JUMP_NE(L_closure_error_args_count);

// ***CODE-GEN LAMBDA-SIMPLE***
  MOV(R1, FPARG(0));
  MOV(R3,IMM(2));
  PUSH(R3);
  CALL(MALLOC);
  DROP(1);
  MOV(R2,R0);
// Copy environments
  MOV(R3, IMM(0));
  MOV(R4, IMM(1));
L_closure_loop_copy_env_lambda_simple_14:
  CMP(R3, IMM(1));
  JUMP_EQ (L_closure_loop_copy_env_end_lambda_simple_14);
  MOV(INDD(R2, R4), INDD(R1, R3));
  INCR(R3);
  INCR(R4);
  JUMP(L_closure_loop_copy_env_lambda_simple_14);
L_closure_loop_copy_env_end_lambda_simple_14:
  CMP(IMM(1), IMM(0));
  JUMP_EQ(L_closure_loop_copy_stack_end_lambda_simple_14)
  MOV(R3,FPARG(1));
  PUSH(R3);
  CALL(MALLOC);
  DROP(1);
  MOV(INDD(R2,0), R0);
// Copy from stack
  MOV(R4, IMM(0));
  MOV(R5, IMM(2));
L_closure_loop_copy_stack_lambda_simple_14:
  CMP(R4, R3);
  JUMP_EQ(L_closure_loop_copy_stack_end_lambda_simple_14);
  MOV(INDD(INDD(R2,0),R4),FPARG(R5));
  INCR(R4);
  INCR(R5);
  JUMP(L_closure_loop_copy_stack_lambda_simple_14);
L_closure_loop_copy_stack_end_lambda_simple_14:
// Create Closure
  PUSH(IMM(3));
  CALL(MALLOC);
  DROP(1);
  MOV(INDD(R0,0),IMM(T_CLOSURE));
  MOV(INDD(R0,1),R2);
  MOV(INDD(R0,2),LABEL(L_closure_body_lambda_simple_14));
  JUMP(L_closure_exit_lambda_simple_14);

// Closure body
L_closure_body_lambda_simple_14:
  PUSH(FP);
  MOV(FP,SP);
  CMP(FPARG(1), IMM(1));
  JUMP_NE(L_closure_error_args_count);
// ***CODE-GEN TC-APPLIC***
  PUSH(SOB_NIL);

// ***CODE-GEN APPLIC***
// Push Parameters
// CODE-GEN pvar
  MOV(R0, FPARG(2));

  PUSH(R0);
// Push Parameters Number
  MOV(R0, IMM(1));
  PUSH(R0);
// Push Closure
// CODE-GEN bvar
  MOV(R1, FPARG(0));
  MOV(R1, INDD(R1, 0));
  MOV(R1, INDD(R1, 0));
  MOV(R0, R1);

  CMP(INDD(R0, 0), IMM(T_CLOSURE));
  JUMP_NE(L_error_cannot_apply_none_closure);
// Push Environment
  PUSH(INDD(R0, 1));
// APPLIC
  CALLA(INDD(R0, 2));
  DROP(1);
  POP(R1);
  DROP(R1);

  PUSH(R0);

  MOV(R0, IMM(1));
  PUSH(R0);
// CODE-GEN bvar
  MOV(R1, FPARG(0));
  MOV(R1, INDD(R1, 0));
  MOV(R1, INDD(R1, 0));
  MOV(R0, R1);

  CMP(INDD(R0,0), IMM(T_CLOSURE));
  JUMP_NE(L_error_cannot_apply_none_closure);
  PUSH(INDD(R0,1));
  PUSH(FPARG(-1));
  MOV(R1,FP);
  MOV(R2,FPARG(1));
  ADD(R2,IMM(5));
  MOV(R3,FP);
  SUB(R3,R2);
  MOV(FP,FPARG(-2));
  MOV(R4,SP);
  DECR(R4);
L_tc_applic_move_stack_12:
  MOV(STACK(R3),STACK(R1));
  INCR(R1);
  INCR(R3);
  CMP(R1,R4);
  JUMP_LE(L_tc_applic_move_stack_12);
  MOV(SP,R3);
  JUMPA(INDD(R0,2));

  POP(FP);
  RETURN;
L_closure_exit_lambda_simple_14:

  POP(FP);
  RETURN;
L_closure_exit_lambda_simple_13:

  CMP(INDD(R0, 0), IMM(T_CLOSURE));
  JUMP_NE(L_error_cannot_apply_none_closure);
// Push Environment
  PUSH(INDD(R0, 1));
// APPLIC
  CALLA(INDD(R0, 2));
  DROP(1);
  POP(R1);
  DROP(R1);

  CMP(INDD(R0, 0), IMM(T_CLOSURE));
  JUMP_NE(L_error_cannot_apply_none_closure);
// Push Environment
  PUSH(INDD(R0, 1));
// APPLIC
  CALLA(INDD(R0, 2));
  DROP(1);
  POP(R1);
  DROP(R1);

  CMP(INDD(R0, 0), IMM(T_CLOSURE));
  JUMP_NE(L_error_cannot_apply_none_closure);
// Push Environment
  PUSH(INDD(R0, 1));
// APPLIC
  CALLA(INDD(R0, 2));
  DROP(1);
  POP(R1);
  DROP(R1);

JUMP(L_errors_end);

L_closure_error_args_count:
  printf("INCORRECT_NUMBER_OF_ARGS. \n");
  JUMP(L_code_finish);

L_error_cannot_apply_none_closure:
  printf("CANNOT APPLY NONE CLOSURE. \n");
  JUMP(L_code_finish);
    
L_error_car_not_pair:
  printf("EXCEPION in CAR: Variable is not a pair. \n");
  JUMP(L_code_finish);
  
L_error_cdr_not_pair:
  printf("EXCEPION in CDR: Variable is not a pair. \n");
  JUMP(L_code_finish);
  
L_error_string_length_not_string:
  printf("EXCEPION in STRING-LENGTH: Variable is not a string. \n");
  JUMP(L_code_finish);
  
L_error_vector_length_not_vector:
  printf("EXCEPION in VECTOR-LENGTH: Variable is not a vector. \n");
  JUMP(L_code_finish);
  
L_error_string_ref_not_string:
  printf("EXCEPION in STRING-REF: 1st variable is not a string. \n");
  JUMP(L_code_finish);
  
L_error_string_ref_not_integer:
  printf("EXCEPION in STRING-REF: 2nd variable is not an integer. \n");
  JUMP(L_code_finish);
  
L_error_vector_ref_not_vector:
  printf("EXCEPION in VECTOR-REF: 1st variable is not a vector. \n");
  JUMP(L_code_finish);
  
L_error_vector_ref_not_integer:
  printf("EXCEPION in VECTOR-REF: 2nd variable is not an integer. \n");
  JUMP(L_code_finish);
  
L_make_vector_not_integer:
  printf("EXCEPION in MAKE-VECTOR: 1st variable is not an integer. \n");
  JUMP(L_code_finish);
  
L_division_by_zero:
  printf("EXCEPION in DIV: Can not devide by 0. \n");
  JUMP(L_code_finish);

L_char2int_not_char:
  printf("EXCEPION in char->integer: Variable is not a char. \n");
  JUMP(L_code_finish);

L_int2char_not_int:
  printf("EXCEPION in integer->char: Variable is not an integer. \n");
  JUMP(L_code_finish);
  
L_error_set_car_not_pair:
  printf("EXCEPION in SET-CAR!: Variable is not a pair. \n");
  JUMP(L_code_finish);
  
L_error_set_cdr_not_pair:
  printf("EXCEPION in SET-CDR!: Variable is not a pair. \n");
  JUMP(L_code_finish);

L_numerator_not_num:
  printf("EXCEPION in NUMERATOR: Variable is not a number. \n");
  JUMP(L_code_finish);
  
L_denominator_not_num:
  printf("EXCEPION in DENOMINATOR: Variable is not a number. \n");
  JUMP(L_code_finish);
  
L_string_set_1_not_string:
  printf("EXCEPION in STRING-SET!: 1st variable is not a string. \n");
  JUMP(L_code_finish);

L_string_set_2_not_int:
  printf("EXCEPION in STRING-SET!: 2nd variable is not an integer. \n");
  JUMP(L_code_finish);

L_string_set_3_not_char:
  printf("EXCEPION in STRING-SET!: 3rd variable is not a char. \n");
  JUMP(L_code_finish);
  
L_vector_set_1_not_vector:
  printf("EXCEPION in VECTOR-SET!: 1st variable is not a vector. \n");
  JUMP(L_code_finish);
  
L_vector_set_2_not_int:
  printf("EXCEPION in VECTOR-SET!: 2nd variable is not an integer. \n");
  JUMP(L_code_finish);
  
L_remainder_not_integer:
  printf("EXCEPION in REMAINDER: variable is not an integer. \n");
  JUMP(L_code_finish);

L_string2symbol_not_string:
  printf("EXCEPION in STRING->SYMBOL variable is not a string. \n");
  JUMP(L_code_finish);
  
L_errors_end:




 CMP(R0,SOB_VOID);
 JUMP_EQ(_VOID);
 PUSH(R0);
 CALL(WRITE_SOB);
 _VOID:
 CALL(NEWLINE);

  DROP(1);


L_code_finish:
  STOP_MACHINE;

  return 0;
} 
