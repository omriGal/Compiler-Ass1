/* fix_fraction.asm
 * function: R0 <- fix_fraction(ARG[0] <fraction>, ARG[1] <gcd>)
 *  Returns Integer or Fraction
 *
 * Programmers: Carmel & Omri
 */

 FIX_FRACTION:
  MOV(R1, STARG(0)); /* ARG[0] = A */
  MOV(R2, INDD(R1,1));
  MOV(R3, INDD(R1,2));
  
  MOV(R4, STARG(1)); /* ARG[1] = B */
  MOV(R4, INDD(R4,1));
  
  DIV(R2, R4);
  DIV(R3, R4);
  
  CMP(R3, IMM(1));
  JUMP_EQ(FIX_TO_INTEGER);
  
  PUSH(R3);
  PUSH(R2);
  CALL(MAKE_SOB_FRACTION);
  DROP(IMM(2))            
  JUMP(FIX_FRACTION_END);


 FIX_TO_INTEGER:
 
  PUSH(R2);
  CALL(MAKE_SOB_INTEGER);
  DROP(IMM(1)) 
  
 FIX_FRACTION_END: 

  RETURN;
