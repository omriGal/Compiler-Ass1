/* gcd.asm
 * Computes GCD function: R0 <- GCD(ARG[0], ARG[1])
 *
 * Programmers: Carmel & Omri
 */

 GCD:
  MOV(R1, STARG(0)); /* ARG[0] = A */
  MOV(R2, STARG(1)); /* ARG[1] = B */
  
 GCD_LOOP:
  CMP(R2, IMM(0));
  JUMP_EQ(SIGN_CHECK);
  MOV(R3, R2);
  REM(R1, R2);
  MOV(R2, R1);
  MOV(R1, R3);
  JUMP(GCD_LOOP);

 SIGN_CHECK:
 
  CMP(R1, IMM(0));
  JUMP_GE(GCD_END);
  
  MUL(R1, IMM(-1));
  
 GCD_END: 
  PUSH(R1);
  CALL(MAKE_SOB_INTEGER);
  DROP(1);  
  
  RETURN;
