#include "ME_RLI_IR_API.h"
#include "ME_RLI_IR.h"

int add(int a, int b) {
  return a + b;
}
int subtract(int a, int b) {
  return a - b;
}

ME_RLI_IR_value ME_RLI_IR_API_add(ME_RLI_IR_value * arg_vals, int arg_count) {
  return ME_RLI_IR_value_create_int(add(arg_vals[0].vdata.int_val,arg_vals[1].vdata.int_val));
}

struct ME_RLI_IR_value ME_RLI_IR_API_subtract(struct ME_RLI_IR_value * arg_vals, int arg_count) {
  return ME_RLI_IR_value_create_int(subtract(arg_vals[0].vdata.int_val,arg_vals[1].vdata.int_val));
}
struct ME_RLI_IR_value ME_RLI_IR_API_strlen(struct ME_RLI_IR_value * arg_vals, int arg_count) {
  return ME_RLI_IR_value_create_int(strlen(arg_vals[0].vdata.string_val));
}
ME_RLI_IR_value ME_RLI_IR_API_print(ME_RLI_IR_value * arg_vals, int arg_count) {
  printf("\nPRINT %s\n",arg_vals[0].vdata.string_val);
  return ME_RLI_IR_value_create_void();
}
ME_RLI_IR_value ME_RLI_IR_API_eq(ME_RLI_IR_value * arg_vals, int arg_count) {
  if (arg_count != 2) {
    printf("eq needs exactly 2 arguments!\n");
    exit(-1);
  }

  if (arg_vals[0].type != arg_vals[1].type)
    return ME_RLI_IR_value_create_int(0);
  if (arg_vals[0].type == ME_RLI_IR_VALUE_INT) {
    if (arg_vals[0].vdata.int_val == arg_vals[1].vdata.int_val)
      return ME_RLI_IR_value_create_int(1);
    else
      return ME_RLI_IR_value_create_int(0);
  } else {
    printf("eq not implemented for this type!\n");
    exit(-1);
  }
}
ME_RLI_IR_value ME_RLI_IR_API_if(ME_RLI_IR_value * args_vals, int arg_count) {
  if (arg_count != 3) {
    printf("if needs exactly 3 arguments!\n");
    exit(-1);
  }
  if (args_vals[0].vdata.int_val)
    return args_vals[1];
  else
    return args_vals[2];
  /*if (args_vals[0].vdata.int_val)
    return ME_RLI_IR_expr_eval(args_vals[1].vdata.lexpr);
  else
  return ME_RLI_IR_expr_eval(args_vals[2].vdata.lexpr);*/
}

struct ME_RLI_IR_value ME_RLI_IR_API_seq(struct ME_RLI_IR_value * args_vals, int arg_count) {
  int i;
  for (i = 0; i < arg_count; i++) {
    ME_RLI_IR_expr_eval(args_vals[i].vdata.lexpr);
  }
  return ME_RLI_IR_value_create_void();
}

ME_RLI_API_func ME_RLI_API_func_look_up(char * func_name) {
  if (strcmp(func_name,"add")==0) return &ME_RLI_IR_API_add;
  else if (strcmp(func_name,"subtract")==0) return &ME_RLI_IR_API_subtract;
  else if (strcmp(func_name,"strlen")==0) return &ME_RLI_IR_API_strlen;
  else if (strcmp(func_name,"print")==0) return &ME_RLI_IR_API_print;
  else if (strcmp(func_name,"seq")==0) return &ME_RLI_IR_API_seq;
  else if (strcmp(func_name,"eq")==0) return &ME_RLI_IR_API_eq;
  else if (strcmp(func_name,"if")==0) return &ME_RLI_IR_API_if;
  else {
    printf("Interpreter: No such function \"%s\"!\n",func_name);
    exit(-1);
  }
}

