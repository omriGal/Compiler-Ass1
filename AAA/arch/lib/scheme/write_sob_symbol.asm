/* scheme/write_sob_symbol.asm
 * Take a pointer to a Scheme integer object, and 
 * prints (to stdout) the character representation
 * of that object.
 * 
 */

WRITE_SOB_SYMBOL:
  PUSH(FP);
  MOV(FP, SP);
  PUSH(R1);
  PUSH(R2);
  PUSH(R3);

  MOV(R0, FPARG(0)); 
  MOV(R5,INDD(R0,1)); 
  MOV(R1, INDD(R5, 1)); 
  MOV(R2, R5);
  ADD(R2, IMM(2));
 L_WSS_LOOP_1:
  CMP(R1, IMM(0));
  JUMP_EQ(L_WSS_EXIT_1);
  CMP(IND(R2), '\n');
  JUMP_EQ(L_WSS_NEWLINE_1);
  CMP(IND(R2), '\t');
  JUMP_EQ(L_WSS_TAB_1);
  CMP(IND(R2), '\f');
  JUMP_EQ(L_WSS_PAGE_1);
  CMP(IND(R2), '\r');
  JUMP_EQ(L_WSS_RETURN_1);
  CMP(IND(R2), '\\');
  JUMP_EQ(L_WSS_BACKSLASH_1);
  CMP(IND(R2), '\"');
  JUMP_EQ(L_WSS_DQUOTE_1);
  CMP(IND(R2), ' ');
  JUMP_LT(L_WSS_OCT_CHAR_1);
  PUSH(IND(R2));
  CALL(PUTCHAR);
  DROP(1);
  JUMP(L_WSS_LOOP_CONT_1);
 L_WSS_DQUOTE_1:
  PUSH(IMM('\\'));
  CALL(PUTCHAR);
  PUSH(IMM('\"'));
  CALL(PUTCHAR);
  DROP(2);
  JUMP(L_WSS_LOOP_CONT_1);
 L_WSS_BACKSLASH_1:
  PUSH(IMM('\\'));
  CALL(PUTCHAR);
  PUSH(IMM('\\'));
  CALL(PUTCHAR);
  DROP(2);
  JUMP(L_WSS_LOOP_CONT_1);
 L_WSS_RETURN_1:
  PUSH(IMM('\\'));
  CALL(PUTCHAR);
  PUSH(IMM('r'));
  CALL(PUTCHAR);
  DROP(2);
  JUMP(L_WSS_LOOP_CONT_1);
 L_WSS_PAGE_1:
  PUSH(IMM('\\'));
  CALL(PUTCHAR);
  PUSH(IMM('f'));
  CALL(PUTCHAR);
  DROP(2);
  JUMP(L_WSS_LOOP_CONT_1);
 L_WSS_TAB_1:
  PUSH(IMM('\\'));
  CALL(PUTCHAR);
  PUSH(IMM('t'));
  CALL(PUTCHAR);
  DROP(2);
  JUMP(L_WSS_LOOP_CONT_1);  
 L_WSS_NEWLINE_1:
  PUSH(IMM('\\'));
  CALL(PUTCHAR);
  PUSH(IMM('n'));
  CALL(PUTCHAR);
  DROP(2);
  JUMP(L_WSS_LOOP_CONT_1);
 L_WSS_OCT_CHAR_1:
  MOV(R0, IND(R2));
  MOV(R3, R0);
  REM(R3, IMM(8));
  PUSH(R3);
  DIV(R0, IMM(8));
  MOV(R3, R0);
  REM(R3, IMM(8));
  PUSH(R3);
  DIV(R0, IMM(8));
  REM(R0, IMM(8));
  PUSH(R0);
  PUSH(IMM('\\'));
  CALL(PUTCHAR);
  DROP(1);
  CALL(WRITE_INTEGER);
  DROP(1);
  CALL(WRITE_INTEGER);
  DROP(1);
  CALL(WRITE_INTEGER);
  DROP(1);
 L_WSS_LOOP_CONT_1:
  INCR(R2);
  DECR(R1);
  JUMP(L_WSS_LOOP_1);
 L_WSS_EXIT_1:

  POP(R3);
  POP(R2);
  POP(R1);
  POP(FP);
  RETURN;

