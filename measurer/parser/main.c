#include "ME_RLI_IR.c"
#include "ME_RLI_IR_API.c"

void main()
{
  ME_RLI_token * tokens = ME_RLI_tokenize("(print \"abc\")");
  ME_RLI_token_print(tokens);

  printf("\n");

  ME_RLI_IR_expr * expr = ME_RLI_IR_expr_parse(&tokens);

  ME_RLI_IR_expr_print(expr);

  ME_RLI_IR_value result = ME_RLI_IR_expr_eval(expr);

  printf("\nValue = ");
  ME_RLI_IR_value_print(result);

  printf("\n");


}
