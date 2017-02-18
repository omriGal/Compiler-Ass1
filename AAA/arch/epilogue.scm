
JUMP(L_errors_end);

L_closure_error_args_count:
  printf("INCORRECT_NUMBER_OF_ARGS \n");
  JUMP(L_code_finish);

L_error_cannot_apply_none_closure:
  printf("CANNOT APPLY NONE CLOSURE \n");
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
