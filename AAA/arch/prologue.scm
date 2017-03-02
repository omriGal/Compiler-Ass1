
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

