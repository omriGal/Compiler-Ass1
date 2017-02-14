
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

  
 CONTINUE:


