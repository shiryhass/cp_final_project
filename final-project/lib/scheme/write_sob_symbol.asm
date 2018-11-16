WRITE_SOB_SYMBOL:
  PUSH(FP);
  MOV(FP, SP);
  PUSH(R1);
  PUSH(R2);
  PUSH(R3);
  MOV(R0, FPARG(0)); //R0 is the symbol address
  ADD(R0, IMM(1));//R0 IS NOW THE STRING OF THAT SYMBOL
  MOV(R0,IND(R0));
  MOV(R1, INDD(R0, 1));//R1 is the size
  MOV(R2, R0);
  ADD(R2, IMM(2)); //R2 points to the beginning of the string
 L_WS_SYMBOL_LOOP:
  CMP(R1, IMM(0));//if no more chars in string EXIT, else
  JUMP_EQ(L_WS_SYMBOL_EXIT);
  PUSH(IND(R2));
  CALL(PUTCHAR);
  DROP(1);
  INCR(R2);
  DECR(R1);
  JUMP(L_WS_SYMBOL_LOOP);
 L_WS_SYMBOL_EXIT:
  POP(R3);
  POP(R2);
  POP(R1);
  POP(FP);
  RETURN;