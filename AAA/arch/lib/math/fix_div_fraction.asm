/* fix_div_fraction.asm
 * function: R0 <- fix_div_fraction(ARG[0] <fraction>)
 *  Returns Fraction- Never with minus in denominator
 *
 * Programmers: Carmel & Omri
 */

 FIX_DIV_FRACTION:
  MOV(R1, STARG(0)); /* ARG[0] = A */
  MOV(R2, INDD(R1,1));
  MOV(R3, INDD(R1,2));
  
  CMP(R3, 0);
  JUMP_GT(FIX_FRACTION_DIV_END);
  
  MUL(R2, IMM(-1));
  MUL(R3, IMM(-1));
  
 FIX_FRACTION_DIV_END: 

  PUSH(R3);
  PUSH(R2);
  CALL(MAKE_SOB_FRACTION);
  DROP(IMM(2))   
  
  RETURN;
