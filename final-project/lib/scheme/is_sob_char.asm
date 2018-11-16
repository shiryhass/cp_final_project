/* scheme/is_sob_char.asm
 * Take pointers to a Scheme object, and places in R0 either 0 or 1
 * (long, not Scheme integer objects or Scheme boolean objets),
 * depending on whether the argument is char.
 * 
 * Programmer: Mayer Goldberg, 2010
 */

 IS_SOB_CHAR1:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R0, FPARG(0));
  CMP(IND(R0), T_CHAR);
  JUMP_EQ(L_IS_SOB_CHAR_TRUE1);
  MOV(R0, IMM(0));
  JUMP(L_IS_SOB_CHAR_EXIT1);
 L_IS_SOB_CHAR_TRUE1:
  MOV(R0, IMM(1));
 L_IS_SOB_CHAR_EXIT1:
  POP(FP);
  RETURN;

