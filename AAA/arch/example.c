/* cisc.c
 * Mock-assembly programming for a CISC-like architecture
 * 
 * Programmer: Mayer Goldberg, 2010
 */

#include <stdio.h>
#include <stdlib.h>

/* change to 0 for no debug info to be printed: */
#define DO_SHOW 1

#include "cisc.h"

int main()
{
  START_MACHINE;

  JUMP(CONTINUE);

#include "char.lib"
#include "io.lib"
#include "math.lib"
#include "string.lib"
#include "system.lib"

 CONTINUE:
  PUSH(IMM(64));
  CALL(MALLOC);
  SHOW("MALLOC RETURNED ", R0);
  DROP(1);
  PUSH(R0);
  OUT(IMM(2), IMM('?'));
  OUT(IMM(2), IMM(' '));
  CALL(READLINE);
  SHOW("READ IN STRING AT ADDRESS ", R0);
  PUSH(R0);
  CALL(STRING_TO_NUMBER);
  DROP(1);
  SHOW("READ IN ", R0);
  MUL(R0, R0);
  SHOW("SQUARE IS ", R0);
  PUSH(R0);
  CALL(NUMBER_TO_STRING);
  DROP(1);
  PUSH(R0);
  SHOW("STR[0] = ", INDD(R0, 0));
  SHOW("STR[1] = ", INDD(R1, 0));
  SHOW("STR[2] = ", INDD(R2, 0));
  SHOW("STR[3] = ", INDD(R3, 0));
  CALL(WRITELN);
  DROP(1);

  STOP_MACHINE;

  return 0;
}
