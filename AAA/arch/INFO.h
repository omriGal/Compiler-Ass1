/* debug_macros.h
 * GDB-Like information for debugging the compiler.
 *
 * Author: Ben Eyal
 */

#define TRANS(x, res) (((x) == T_VOID) ? snprintf(res, 16, "%s", "T_VOID")     \
  : ((x) == T_NIL)     ? snprintf(res, 16, "%s", "T_NIL")                      \
  : ((x) == T_BOOL)    ? snprintf(res, 16, "%s", "T_BOOL")                     \
  : ((x) == T_CHAR)    ? snprintf(res, 16, "%s", "T_CHAR")                     \
  : ((x) == T_INTEGER) ? snprintf(res, 16, "%s", "T_INTEGER")                  \
  : ((x) == T_STRING)  ? snprintf(res, 16, "%s", "T_STRING")                   \
  : ((x) == T_SYMBOL)  ? snprintf(res, 16, "%s", "T_SYMBOL")                   \
  : ((x) == T_PAIR)    ? snprintf(res, 16, "%s", "T_PAIR")                     \
  : ((x) == T_VECTOR)  ? snprintf(res, 16, "%s", "T_VECTOR")                   \
  : ((x) == T_CLOSURE) ? snprintf(res, 16, "%s", "T_CLOSURE")                  \
  : ((x) == T_FRACTION) ? snprintf(res, 16, "%s", "T_FRACTION")                  \
  : snprintf(res, 16, "%ld", (x)))

#if DO_SHOW == 0
#define INFO {}
#else
#define INFO {                                                                      \
  char type1[16];                                                                   \
  char type2[16];                                                                   \
  char type3[16];                                                                   \
  char type4[16];                                                                   \
  printf("\n");                                                                     \
  printf("----------------------------\n");                                         \
  printf("Register Info:\n");                                                       \
  printf("----------------------------\n");                                         \
  printf("FP = %-6ld SP = %ld\n\n", FP, SP);                                        \
  TRANS(R0, type1); TRANS(R1, type2);                                               \
  printf("R0 = %-10s R1 = %s\n", type1, type2);                                     \
  TRANS(R2, type1); TRANS(R3, type2);                                               \
  printf("R2 = %-10s R3 = %s\n", type1, type2);                                     \
  TRANS(R4, type1); TRANS(R5, type2);                                               \
  printf("R4 = %-10s R5 = %s\n\n", type1, type2);                                   \
  TRANS(R6, type1); TRANS(R7, type2);                                               \
  printf("R6 = %-10s R7 = %s\n\n", type1, type2);                                   \
  TRANS(R8, type1); TRANS(R9, type2);                                               \
  printf("R8 = %-10s R9 = %s\n\n", type1, type2);                                   \
 TRANS(R10, type1); TRANS(R11, type2);                                               \
 printf("R10 = %-10s R11 = %s\n\n", type1, type2);                                   \
  TRANS(R12, type1); TRANS(R13, type2);                                               \
 printf("R12 = %-10s R13 = %s\n\n", type1, type2);                                   \
  TRANS(R14, type1); TRANS(R15, type2);                                               \
 printf("R14 = %-10s R15 = %s\n\n", type1, type2);                                   \
                                                                                    \
  printf("----------------------------\n");                                         \
  printf("Stack Info:\n");                                                          \
  printf("----------------------------\n");                                         \
  int i;                                                                            \
  for (i = SP; i >= 0; i--) {                                                       \
    TRANS(STACK(i), type1);                                                         \
    printf("STACK[%2d] = %s\n", i, type1);                                          \
  }                                                                                 \
  printf("\n");                                                                     \
  printf("----------------------------\n");                                         \
  printf("Memory Info:\n");                                                         \
  printf("----------------------------\n");                                         \
  for (i = 0; i <= 250; i += 4) {                                               \
    TRANS(IND(i), type1); TRANS(IND(i + 1), type2);                                 \
    TRANS(IND(i + 2) ,type3); TRANS(IND(i + 3), type4);                             \
    printf("MEM[%4d] = %-10s MEM[%d] = %-10s MEM[%4d] = %-10s MEM[%d] = %-10s\n",   \
        i, type1,                                                                   \
        i + 1, type2,                                                               \
        i + 2, type3,                                                               \
        i + 3, type4);                                                              \
  }                                                                                 \
  printf("\n");                                                                     \
}
#endif 
