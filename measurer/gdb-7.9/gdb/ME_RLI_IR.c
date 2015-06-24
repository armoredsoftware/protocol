#include<stdio.h>
#include<stdlib.h>
#include<string.h>

#include "ME_RLI_IR.h"
#include "ME_RLI_IR_API.h"

/*typedef struct ME_RLI_token {
  char value[MAX_TOKEN_LENGTH];
  struct ME_RLI_token * next;
  } ME_RLI_token;*/

typedef enum ME_RLI_IR_value_type {
  ME_RLI_IR_VALUE_INT, ME_RLI_IR_VALUE_STRING, ME_RLI_IR_VALUE_VOID, ME_RLI_IR_VALUE_LEXPR,
  ME_RLI_IR_VALUE_MEASUREMENT, ME_RLI_IR_VALUE_EVENT, ME_RLI_IR_VALUE_FEATURE, ME_RLI_IR_VALUE_ERROR
}
ME_RLI_IR_value_type;

typedef struct ME_RLI_IR_value {
  ME_RLI_IR_value_type type; 
  union vdata {
    int int_val;
    char string_val[MAX_STRING_LENGTH];
    struct ME_RLI_IR_expr * lexpr;
    struct ME_measurement * ms;
    struct BE_event * event;
    struct ME_feature * feature;
    char error_desc[MAX_STRING_LENGTH];
  } vdata;
}
ME_RLI_IR_value;

typedef enum ME_RLI_IR_expr_type {
  ME_RLI_IR_EXPR_VALUE, ME_RLI_IR_EXPR_FUNC
}
ME_RLI_IR_expr_type;

typedef struct ME_RLI_IR_expr {
  ME_RLI_IR_expr_type type;
  union expr_data {
    struct ME_RLI_IR_value * value;
    struct ME_RLI_IR_func * func;
  } data;
}
ME_RLI_IR_expr;

/*typedef struct ME_RLI_IR_arg {
  ME_RLI_IR_expr * expr;
  struct ME_RLI_IR_arg * next;
}
ME_RLI_IR_arg;

typedef struct ME_RLI_IR_func {
  struct ME_RLI_IR_sym * func_name;
  struct ME_RLI_IR_arg * args;    
}
ME_RLI_IR_func;*/

typedef struct ME_RLI_IR_sym {
  char value[MAX_SYM_LENGTH];
}
ME_RLI_IR_sym;

/*====================================================
  TOKEN STUFF
  ====================================================*/

ME_RLI_token * ME_RLI_token_create(char * value) {
  ME_RLI_token * token = (ME_RLI_token*)malloc(sizeof(ME_RLI_token));
  strncpy(token->value, value, MAX_TOKEN_LENGTH);
  token->next = NULL;
  return token;
}

void ME_RLI_token_print(ME_RLI_token * token) {
  if (!token) return;

  printf("%s", token->value);

  if (token->next) {
    printf(",");
    ME_RLI_token_print(token->next);
  }
}

bool _white_space(char c) {
  return c == ' ' || c == '\n' || c == '\t';
}

ME_RLI_token * ME_RLI_tokenize(char * in) {
  int curr = 0;
  char token[MAX_TOKEN_LENGTH];
  int token_curr = 0;

  ME_RLI_token * tokens = NULL;
  ME_RLI_token * tokens_curr = NULL;

  //TODO ensure token length....
  
  while (1) {
     
    //consume token
    if (in[curr] == '(' || in[curr] == ')') {
      token[token_curr++] = in[curr++];
    } else if (in[curr] == '"') {
      token[token_curr++] = in[curr++]; 
      while (in[curr] != '"') {
	if (in[curr] == 0) { 
	  printf("Lexer error: mismatched quotes\n");
	  return NULL;
	}
	token[token_curr++] = in[curr++]; 
      }
      token[token_curr++] = in[curr++]; 
    } else {
      while (!_white_space(in[curr]) && in[curr] != 0 && in[curr] != '(' && in[curr] != ')') {
	token[token_curr++] = in[curr++]; 
      }      
    }

    //commit token
    token[token_curr] = 0;
    //printf("Token %s\n", token);
    token_curr = 0;
    if (tokens_curr) {
      tokens_curr->next = ME_RLI_token_create(token);
      tokens_curr = tokens_curr->next;
    } else {
      tokens = ME_RLI_token_create(token);
      tokens_curr = tokens;
    }
      
    //check if all characters are consumed
    if (in[curr] == 0) {
      break;
    }
  
    //consume trailing whitespace
    while (_white_space(in[curr])) {
      curr++;
    }

  }
  
  return tokens;
}

/*====================================================
  VALUE STUFF
  ====================================================*/

ME_RLI_IR_value ME_RLI_IR_value_create_int(int int_val) {
  struct ME_RLI_IR_value value;
  value.type = ME_RLI_IR_VALUE_INT;
  value.vdata.int_val = int_val;
  return value;
}
ME_RLI_IR_value ME_RLI_IR_value_get_int(ME_RLI_IR_value value, int * get) {
  if (value.type != ME_RLI_IR_VALUE_INT) {
    return ME_RLI_IR_value_create_error("Expected an int!");
  }
  (*get) = value.vdata.int_val;
  return ME_RLI_IR_value_create_void();
}

ME_RLI_IR_value ME_RLI_IR_value_create_string(char * string_val) {
  struct ME_RLI_IR_value value;
  value.type = ME_RLI_IR_VALUE_STRING;
  strncpy(value.vdata.string_val, string_val,MAX_STRING_LENGTH);
  return value;
}
ME_RLI_IR_value ME_RLI_IR_value_get_string(ME_RLI_IR_value value, char ** get) {
  if (value.type != ME_RLI_IR_VALUE_STRING) {
    return ME_RLI_IR_value_create_error("Expected a string!");
  }
  (*get) = value.vdata.string_val;
  return ME_RLI_IR_value_create_void();
}


ME_RLI_IR_value ME_RLI_IR_value_create_error(char * error_desc) {
  struct ME_RLI_IR_value value;
  value.type = ME_RLI_IR_VALUE_ERROR;
  strncpy(value.vdata.error_desc, error_desc, MAX_STRING_LENGTH);
  return value;
}

ME_RLI_IR_value ME_RLI_IR_value_create_measurement(struct ME_measurement * ms) {
  struct ME_RLI_IR_value value;
  value.type = ME_RLI_IR_VALUE_MEASUREMENT;
  value.vdata.ms = ms;
  return value;
}
ME_RLI_IR_value ME_RLI_IR_value_get_measurement(ME_RLI_IR_value value, ME_measurement ** get) {
  if (value.type != ME_RLI_IR_VALUE_MEASUREMENT) {
    return ME_RLI_IR_value_create_error("Expected a measurement!");
  }
  (*get) = value.vdata.ms;
  return ME_RLI_IR_value_create_void();
}

ME_RLI_IR_value ME_RLI_IR_value_create_event(struct BE_event * event) {
  struct ME_RLI_IR_value value;
  value.type = ME_RLI_IR_VALUE_EVENT;
  value.vdata.event = event;
  return value;
}
ME_RLI_IR_value ME_RLI_IR_value_get_event(ME_RLI_IR_value value, BE_event ** get) {
  if (value.type != ME_RLI_IR_VALUE_EVENT) {
    return ME_RLI_IR_value_create_error("Expected an event!");
  }
  (*get) = value.vdata.event;
  return ME_RLI_IR_value_create_void();
}

ME_RLI_IR_value ME_RLI_IR_value_create_feature(struct ME_feature * feature) {
  struct ME_RLI_IR_value value;
  value.type = ME_RLI_IR_VALUE_FEATURE;
  value.vdata.feature = feature;
  return value;
}
ME_RLI_IR_value ME_RLI_IR_value_get_feature(ME_RLI_IR_value value, struct ME_feature ** get) {
  if (value.type != ME_RLI_IR_VALUE_FEATURE) {
    return ME_RLI_IR_value_create_error("Expected a feature!");
  }
  (*get) = value.vdata.feature;
  return ME_RLI_IR_value_create_void();
}

ME_RLI_IR_value ME_RLI_IR_value_create_void() {
  struct ME_RLI_IR_value value;
  value.type = ME_RLI_IR_VALUE_VOID;
  return value;
}

ME_RLI_IR_value ME_RLI_IR_value_create_lexpr(struct ME_RLI_IR_expr * lexpr) {
  struct ME_RLI_IR_value value;
  value.type = ME_RLI_IR_VALUE_LEXPR;
  value.vdata.lexpr = lexpr;
  return value;
}
ME_RLI_IR_value ME_RLI_IR_value_get_lexpr(ME_RLI_IR_value value, struct ME_RLI_IR_expr ** get) {
  if (value.type != ME_RLI_IR_VALUE_LEXPR) {
    return ME_RLI_IR_value_create_error("Expected a expression!");
  }
  (*get) = value.vdata.lexpr;
  return ME_RLI_IR_value_create_void();
}


void ME_RLI_IR_value_print(ME_RLI_IR_value value) {
  if (value.type == ME_RLI_IR_VALUE_INT) {
    printf("INT:%d",value.vdata.int_val);
  } else if (value.type == ME_RLI_IR_VALUE_STRING) {
    printf("STRING:\"%s\"",value.vdata.string_val);
  } else if (value.type == ME_RLI_IR_VALUE_VOID) {
    printf("VOID");
  } else if (value.type == ME_RLI_IR_VALUE_LEXPR) {
    printf("LEXPR:");
    ME_RLI_IR_expr_print(value.vdata.lexpr);
  } else if (value.type == ME_RLI_IR_VALUE_MEASUREMENT) {
    printf("MEASUREMENT:");
    ME_measurement_print(value.vdata.ms);
  } else if (value.type == ME_RLI_IR_VALUE_EVENT) {
    printf("EVENT:");
    BE_event_print(value.vdata.event);
  } else if (value.type == ME_RLI_IR_VALUE_FEATURE) {
    printf("FEATURE:");
    ME_feature_print(value.vdata.feature);
  } else if (value.type == ME_RLI_IR_VALUE_ERROR) {
    printf("ERROR:\"%s\"",value.vdata.error_desc);
  }
}

ME_RLI_IR_value * ME_RLI_IR_value_parse(ME_RLI_token ** curr) {
  ME_RLI_IR_value * value;
  ME_RLI_IR_expr * lexpr;
  int int_val;
  
  value = (ME_RLI_IR_value*)malloc(sizeof(ME_RLI_IR_value));
  
  //if string pattern detected
  if (strcmp((*curr)->value,"'")==0) {
    //consume first tick
    (*curr) = (*curr)->next;

    lexpr = ME_RLI_IR_expr_parse(curr);
    if (!lexpr) return NULL;
    
    (*value) = ME_RLI_IR_value_create_lexpr(lexpr);
    
  } else if ((*curr)->value[0] == '"') {
    char temp[strlen((*curr)->value)-1];
    strcpy(temp,(*curr)->value+1);
    temp[strlen(temp)-1] = 0;
    
    (*value) = ME_RLI_IR_value_create_string(temp);
    (*curr) = (*curr)->next;
  } else {

    int_val = atoi((*curr)->value);
    (*curr) = (*curr)->next;
    (*value) = ME_RLI_IR_value_create_int(int_val);
    
  }
  
  return value;
}

/*====================================================
  EXPR STUFF
  ====================================================*/

ME_RLI_IR_expr * ME_RLI_IR_expr_create_value(ME_RLI_IR_value * value) {
  if (!value) return NULL;
  ME_RLI_IR_expr * expr = (ME_RLI_IR_expr*)(malloc(sizeof(ME_RLI_IR_expr)));
  expr->type = ME_RLI_IR_EXPR_VALUE;
  expr->data.value = value;
  return expr;
}

ME_RLI_IR_expr * ME_RLI_IR_expr_create_func(ME_RLI_IR_func * func) {
  if (!func) return NULL;
  ME_RLI_IR_expr * expr = (ME_RLI_IR_expr*)(malloc(sizeof(ME_RLI_IR_expr)));
  expr->type = ME_RLI_IR_EXPR_FUNC;
  expr->data.func = func;
  return expr;
}

void ME_RLI_IR_expr_print(ME_RLI_IR_expr * expr)
{
  if (expr->type == ME_RLI_IR_EXPR_VALUE) {
    ME_RLI_IR_value_print(*(expr->data.value));
  }
  else if (expr->type == ME_RLI_IR_EXPR_FUNC) {
    ME_RLI_IR_func_print(expr->data.func);
  }
}

ME_RLI_IR_expr * ME_RLI_IR_expr_parse(ME_RLI_token ** curr) {

  ME_RLI_IR_expr * expr = NULL;
  
  if (strcmp((*curr)->value,"(")==0) {
    expr = ME_RLI_IR_expr_create_func(ME_RLI_IR_func_parse(curr));
  } else {
    expr = ME_RLI_IR_expr_create_value(ME_RLI_IR_value_parse(curr));
  }
  /*printf("Parser error: expected an Expression at token %s!\n",(*curr)->value);
    exit(-1);*/  
  return expr;
}

ME_RLI_IR_value ME_RLI_IR_expr_eval(ME_RLI_IR_expr * expr) {
  if (expr->type == ME_RLI_IR_EXPR_VALUE) {
    return (*expr->data.value);
  }
  else if (expr->type == ME_RLI_IR_EXPR_FUNC) {
    return ME_RLI_IR_func_eval(expr->data.func);
    
  }
}

/*====================================================
  FUNC STUFF
  ====================================================*/

ME_RLI_IR_func * ME_RLI_IR_func_create(ME_RLI_IR_sym * func_name) {
  ME_RLI_IR_func * func = (ME_RLI_IR_func*)(malloc(sizeof(ME_RLI_IR_func)));
  func->func_name = func_name;
  func->args = NULL;
  return func;
}

void ME_RLI_IR_func_print(ME_RLI_IR_func * func)
{
  printf("(");
  ME_RLI_IR_sym_print(func->func_name);

  ME_RLI_IR_arg * arg_curr = func->args;

  while (arg_curr) {
    printf(" ");
    ME_RLI_IR_expr_print(arg_curr->expr);
    arg_curr = arg_curr->next;
  }
  
  printf(")");
}

int ME_RLI_IR_func_arg_count(ME_RLI_IR_func * func)
{
  ME_RLI_IR_arg * arg_curr = func->args;
  int count = 0;
  
  while (arg_curr) {
    arg_curr = arg_curr->next;
    count++;
  }

  return count;
}

void ME_RLI_IR_func_add_arg(ME_RLI_IR_func * func, ME_RLI_IR_expr * expr) {
  ME_RLI_IR_arg * arg;
  ME_RLI_IR_arg * curr;
  
  arg = (ME_RLI_IR_arg*)malloc(sizeof(ME_RLI_IR_arg));

  arg->expr = expr;
  arg->next = NULL;
  
  if (!func->args) {
    func->args = arg;
    return;
  }
  
  curr = func->args;
  while(curr->next) {
    curr = curr->next;
  }

  curr->next = arg;
}

struct ME_RLI_IR_func * ME_RLI_IR_func_parse(struct ME_RLI_token ** curr) {
  //consume parenthesis
  if (strcmp((*curr)->value,"(")!=0) {
    printf("Parse error: expected \"(\"");
    exit(-1);
  }
  (*curr) = (*curr)->next;

  //consume function name
  ME_RLI_IR_sym * func_name = ME_RLI_IR_sym_parse(curr);

  ME_RLI_IR_func * func = ME_RLI_IR_func_create(func_name);

  //consume arguments
  while ((*curr) && strcmp((*curr)->value,")")!=0) {
    //parse expression
    ME_RLI_IR_expr * arg = ME_RLI_IR_expr_parse(curr);
    if (!arg) {
      return NULL;
    }    
    ME_RLI_IR_func_add_arg(func, arg);
  }

  //consume parenthesis
  if (!(*curr) || strcmp((*curr)->value,")")!=0) {
    printf("Parse error: Mismatched parenthesis \")\"\n");
    return NULL;
  }  
  (*curr) = (*curr)->next;
  
  return func;
}

ME_RLI_IR_value ME_RLI_IR_func_eval(ME_RLI_IR_func * func) {
  int args_count, i;
  
  //evaluate arguments
  args_count = ME_RLI_IR_func_arg_count(func);
  ME_RLI_IR_value arg_vals[args_count];
  i = 0;
  ME_RLI_IR_arg * arg_curr = func->args;
  for (i = 0; i<args_count; i++) {
    arg_vals[i] = ME_RLI_IR_expr_eval(arg_curr->expr);

    //If error, return
    if (arg_vals[i].type == ME_RLI_IR_VALUE_ERROR) {
      return arg_vals[i];
    }
    
    arg_curr = arg_curr->next;
  }
  //resolve function name and evaluate
  ME_RLI_API_func f = ME_RLI_API_func_look_up(func->func_name->value);
  if (!f) return ME_RLI_IR_value_create_error("No such function!");
  ME_RLI_IR_value result;
  result = (*f)(arg_vals,args_count);
  return result;
}

/*====================================================
  SYM STUFF
  ====================================================*/

ME_RLI_IR_sym * ME_RLI_IR_sym_create(char * value) {
  ME_RLI_IR_sym * sym = (ME_RLI_IR_sym*)(malloc(sizeof(ME_RLI_IR_sym)));
  strncpy(sym->value, value, MAX_SYM_LENGTH);
  return sym;
}

void ME_RLI_IR_sym_print(ME_RLI_IR_sym * sym)
{
  printf("%s",sym->value);
}

ME_RLI_IR_sym * ME_RLI_IR_sym_parse(ME_RLI_token ** curr) {
  ME_RLI_IR_sym * sym = ME_RLI_IR_sym_create((*curr)->value);
  (*curr) = (*curr)->next;
  return sym;
}
