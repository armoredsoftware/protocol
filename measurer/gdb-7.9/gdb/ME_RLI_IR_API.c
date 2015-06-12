#include "ME_RLI_IR_API.h"
#include "ME_RLI_IR.h"

ME_RLI_IR_value ME_RLI_IR_API_add(ME_RLI_IR_value * arg_vals, int arg_count) {
  return ME_RLI_IR_value_create_int(ME_API_add(arg_vals[0].vdata.int_val,arg_vals[1].vdata.int_val));
}

struct ME_RLI_IR_value ME_RLI_IR_API_subtract(struct ME_RLI_IR_value * arg_vals, int arg_count) {
  return ME_RLI_IR_value_create_int(ME_API_subtract(arg_vals[0].vdata.int_val,arg_vals[1].vdata.int_val));
}
struct ME_RLI_IR_value ME_RLI_IR_API_strlen(struct ME_RLI_IR_value * arg_vals, int arg_count) {
  return ME_RLI_IR_value_create_int(strlen(arg_vals[0].vdata.string_val));
}
ME_RLI_IR_value ME_RLI_IR_API_print(ME_RLI_IR_value * arg_vals, int arg_count) {
  ME_RLI_IR_value_print(arg_vals[0]);
  printf("\n");
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

struct ME_RLI_IR_value ME_RLI_IR_API_set_target(struct ME_RLI_IR_value * args_vals, int arg_count) {

  ME_API_set_target(args_vals[0].vdata.string_val);
 
  return ME_RLI_IR_value_create_void();
}

struct ME_RLI_IR_value ME_RLI_IR_API_detach(struct ME_RLI_IR_value * args_vals, int arg_count) {
  ME_API_detach();
  return ME_RLI_IR_value_create_void();
}

struct ME_RLI_IR_value ME_RLI_IR_API_quit(struct ME_RLI_IR_value * args_vals, int arg_count) {
  ME_API_quit();
  return ME_RLI_IR_value_create_void();
}

struct ME_RLI_IR_value ME_RLI_IR_API_print_context(struct ME_RLI_IR_value * args_vals, int arg_count) {
  ME_API_print_context();
  return ME_RLI_IR_value_create_void();
}

struct ME_RLI_IR_value ME_RLI_IR_API_measure_callstack(struct ME_RLI_IR_value * args_vals, int arg_count) {
  ME_measurement * ms = ME_API_measure_callstack();
  return ME_RLI_IR_value_create_measurement(ms);
}

struct ME_RLI_IR_value ME_RLI_IR_API_measure(struct ME_RLI_IR_value * args_vals, int arg_count) {
  BE_feature * feature = args_vals[0].vdata.feature;
  ME_measurement * ms = ME_API_measure(feature);
  return ME_RLI_IR_value_create_measurement(ms);
}

struct ME_RLI_IR_value ME_RLI_IR_API_sendme(struct ME_RLI_IR_value * args_vals, int arg_count) {
  ME_measurement * ms = args_vals[0].vdata.ms;
  ME_API_sendme(ms);
  return ME_RLI_IR_value_create_void();
}

struct ME_RLI_IR_value ME_RLI_IR_API_store(struct ME_RLI_IR_value * args_vals, int arg_count) {
   int i = args_vals[0].vdata.int_val;
   ME_measurement * ms = args_vals[1].vdata.ms;
   ME_API_store(i,ms);
   return ME_RLI_IR_value_create_void();
}

struct ME_RLI_IR_value ME_RLI_IR_API_load(struct ME_RLI_IR_value * args_vals, int arg_count) {
  int i = args_vals[0].vdata.int_val;
  ME_measurement * ms = ME_API_load(i);
  return ME_RLI_IR_value_create_measurement(ms);
}

struct ME_RLI_IR_value ME_RLI_IR_API_delay(struct ME_RLI_IR_value * args_vals, int arg_count) {
  int delay = args_vals[0].vdata.int_val;
  int repeat = args_vals[1].vdata.int_val;
  BE_event * event = ME_API_delay(delay, repeat);
  return ME_RLI_IR_value_create_event(event);
}

struct ME_RLI_IR_value ME_RLI_IR_API_reach(struct ME_RLI_IR_value * args_vals, int arg_count) {
  char * filename = args_vals[0].vdata.string_val;
  int line = args_vals[1].vdata.int_val;
  int repeat = args_vals[2].vdata.int_val;
  BE_event * event = ME_API_reach(filename, line, repeat);
  return ME_RLI_IR_value_create_event(event);
}

struct ME_RLI_IR_value ME_RLI_IR_API_hook(struct ME_RLI_IR_value * args_vals, int arg_count) {
  BE_event * event = args_vals[0].vdata.event;
  ME_RLI_IR_expr * action = args_vals[1].vdata.lexpr;
  int i = ME_API_hook(event, action);
  return ME_RLI_IR_value_create_int(i);
}

struct ME_RLI_IR_value ME_RLI_IR_API_callstack(struct ME_RLI_IR_value * args_vals, int arg_count) {
  //char * filename = args_vals[0].vdata.string_val;
  BE_feature * feature = ME_API_callstack();
  return ME_RLI_IR_value_create_feature(feature);
}

struct ME_RLI_IR_value ME_RLI_IR_API_var(struct ME_RLI_IR_value * args_vals, int arg_count) {
  char * var_name = args_vals[0].vdata.string_val;
  BE_feature * feature = ME_API_var(var_name);
  return ME_RLI_IR_value_create_feature(feature);
}

struct ME_RLI_IR_value ME_RLI_IR_API_kill(struct ME_RLI_IR_value * args_vals, int arg_count) {
  int i = args_vals[0].vdata.int_val;
  ME_API_kill(i);
  return ME_RLI_IR_value_create_void();
}

struct ME_RLI_IR_value ME_RLI_IR_API_enable(struct ME_RLI_IR_value * args_vals, int arg_count) {
  int i = args_vals[0].vdata.int_val;
  ME_API_enable(i);
  return ME_RLI_IR_value_create_void();
}

struct ME_RLI_IR_value ME_RLI_IR_API_disable(struct ME_RLI_IR_value * args_vals, int arg_count) {
  int i = args_vals[0].vdata.int_val;
  ME_API_disable(i);
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
  else if (strcmp(func_name,"set_target")==0) return &ME_RLI_IR_API_set_target;
  else if (strcmp(func_name,"detach")==0) return &ME_RLI_IR_API_detach;
  else if (strcmp(func_name,"quit")==0) return &ME_RLI_IR_API_quit;
  else if (strcmp(func_name,"print_context")==0) return &ME_RLI_IR_API_print_context;
  else if (strcmp(func_name,"measure_callstack")==0) return &ME_RLI_IR_API_measure_callstack;
  else if (strcmp(func_name,"measure")==0) return &ME_RLI_IR_API_measure;
  else if (strcmp(func_name,"sendme")==0) return &ME_RLI_IR_API_sendme;
  else if (strcmp(func_name,"store")==0) return &ME_RLI_IR_API_store;
  else if (strcmp(func_name,"load")==0) return &ME_RLI_IR_API_load;
  else if (strcmp(func_name,"delay")==0) return &ME_RLI_IR_API_delay;
  else if (strcmp(func_name,"reach")==0) return &ME_RLI_IR_API_reach;
  else if (strcmp(func_name,"hook")==0) return &ME_RLI_IR_API_hook;
  else if (strcmp(func_name,"callstack")==0) return &ME_RLI_IR_API_callstack;
  else if (strcmp(func_name,"var")==0) return &ME_RLI_IR_API_var;
  else if (strcmp(func_name,"kill")==0) return &ME_RLI_IR_API_kill;
  else if (strcmp(func_name,"enable")==0) return &ME_RLI_IR_API_enable;
  else if (strcmp(func_name,"disable")==0) return &ME_RLI_IR_API_disable;
  else {
    printf("Interpreter: No such function \"%s\"!\n",func_name);
    exit(-1);
  }
}

