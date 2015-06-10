#include<stdio.h>
#include<stdlib.h>
#include<string.h>

#include "ME_RLI_IR.h"
#include "ME_RLI_IR_API.h"

typedef struct ME_RLI_token {
  char value[MAX_TOKEN_LENGTH];
  struct ME_RLI_token * next;
} ME_RLI_token;

typedef enum ME_RLI_IR_value_type {
  ME_RLI_IR_VALUE_INT, ME_RLI_IR_VALUE_STRING, ME_RLI_IR_VALUE_VOID, ME_RLI_IR_VALUE_LEXPR
}
ME_RLI_IR_value_type;

typedef struct ME_RLI_IR_value {
  ME_RLI_IR_value_type type; 
  union vdata {
    int int_val;
    char string_val[MAX_STRING_LENGTH];
    struct ME_RLI_IR_expr * lexpr;
  } vdata;
}
ME_RLI_IR_value;

typedef enum ME_RLI_IR_expr_type {
  ME_RLI_IR_EXPR_VALUE, ME_RLI_IR_EXPR_FUNC
}
ME_RLI_IR_expr_type;

typedef struct ME_RLI_IR_expr {
  ME_RLI_IR_expr_type type;
  union data {
    struct ME_RLI_IR_value * value;
    struct ME_RLI_IR_func * func;
  } data;
}
ME_RLI_IR_expr;

typedef struct ME_RLI_IR_arg {
  ME_RLI_IR_expr * expr;
  struct ME_RLI_IR_arg * next;
}
ME_RLI_IR_arg;

typedef struct ME_RLI_IR_func {
  struct ME_RLI_IR_sym * func_name;
  struct ME_RLI_IR_arg * args;    
}
ME_RLI_IR_func;

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

ME_RLI_token * ME_RLI_tokenize(char * in) {
  int curr = 0;
  char token[MAX_TOKEN_LENGTH];
  char token_curr = 0;

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
	  exit(-1);
	}
	token[token_curr++] = in[curr++]; 
      }
      token[token_curr++] = in[curr++]; 
    } else {
      while (in[curr] != ' ' && in[curr] != 0 && in[curr] != '(' && in[curr] != ')') {
	token[token_curr++] = in[curr++]; 
      }      
    }
    //commit token
    token[token_curr] = 0;
    printf("Token %s\n", token);
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
    while (in[curr] == ' ') {
      in[curr++];
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

ME_RLI_IR_value ME_RLI_IR_value_create_string(char * string_val) {
  struct ME_RLI_IR_value value;
  value.type = ME_RLI_IR_VALUE_STRING;
  strncpy(value.vdata.string_val, string_val,MAX_STRING_LENGTH);
  return value;
}

ME_RLI_IR_value ME_RLI_IR_value_create_void() {
  struct ME_RLI_IR_value value;
  value.type = ME_RLI_IR_VALUE_VOID;
  return value;
}

ME_RLI_IR_value ME_RLI_IR_value_create_lexpr(ME_RLI_IR_expr * lexpr) {
  struct ME_RLI_IR_value value;
  value.type = ME_RLI_IR_VALUE_LEXPR;
  value.vdata.lexpr = lexpr;
  return value;
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
  }
}

ME_RLI_IR_value * ME_RLI_IR_value_parse(ME_RLI_token ** curr) {
  ME_RLI_IR_value * value = (ME_RLI_IR_value*)malloc(sizeof(ME_RLI_IR_value));

  //if string pattern detected
  if (strcmp((*curr)->value,"'")==0) {
    //consume first tick
    (*curr) = (*curr)->next;

    ME_RLI_IR_expr * lexpr = ME_RLI_IR_expr_parse(curr);
    (*value) = ME_RLI_IR_value_create_lexpr(lexpr);
    
  } else if ((*curr)->value[0] == '"') {
    char temp[strlen((*curr)->value)-1];
    strcpy(temp,(*curr)->value+1);
    temp[strlen(temp)-1] = 0;
    
    (*value) = ME_RLI_IR_value_create_string(temp);
    (*curr) = (*curr)->next;
  } else {

    int int_val = atoi((*curr)->value);
    (*curr) = (*curr)->next;
    (*value) = ME_RLI_IR_value_create_int(int_val);

  }
  
  return value;
}

/*====================================================
  EXPR STUFF
  ====================================================*/

ME_RLI_IR_expr * ME_RLI_IR_expr_create_value(ME_RLI_IR_value * value) {
  ME_RLI_IR_expr * expr = (ME_RLI_IR_expr*)(malloc(sizeof(ME_RLI_IR_expr)));
  expr->type = ME_RLI_IR_EXPR_VALUE;
  expr->data.value = value;
  return expr;
}

ME_RLI_IR_expr * ME_RLI_IR_expr_create_func(ME_RLI_IR_func * func) {
  ME_RLI_IR_expr * expr = (ME_RLI_IR_expr*)(malloc(sizeof(ME_RLI_IR_expr)));
  expr->type = ME_RLI_IR_EXPR_FUNC;
  expr->data.func = func;
  return expr;
}

void ME_RLI_IR_expr_print(ME_RLI_IR_expr * expr)
{
  if (expr->type == ME_RLI_IR_EXPR_VALUE) {
    ME_RLI_IR_value_print((*expr->data.value));
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
  ME_RLI_IR_arg * arg = (ME_RLI_IR_arg*)malloc(sizeof(ME_RLI_IR_arg));
  arg->expr = expr;

  if (!func->args) {
    func->args = arg;
    return;
  }
  
  ME_RLI_IR_arg * curr = func->args;
  while(curr->next) {
    curr = curr->next;
  }

  curr->next = arg;
}

ME_RLI_IR_func * ME_RLI_IR_func_parse(ME_RLI_token ** curr) {
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

    ME_RLI_IR_func_add_arg(func, arg);
  }

  //consume parenthesis
  if (!(*curr) || strcmp((*curr)->value,")")!=0) {
    printf("Parse error: Mismatched parenthesis \")\"\n");
    exit(-1);
  }  
  (*curr) = (*curr)->next;
  
  return func;
}

ME_RLI_IR_value ME_RLI_IR_func_eval(ME_RLI_IR_func * func) {
  //evaluate arguments
  int args_count = ME_RLI_IR_func_arg_count(func);
  ME_RLI_IR_value arg_vals[args_count];
  int i = 0;
  ME_RLI_IR_arg * arg_curr = func->args;
  for (i = 0; i<args_count; i++) {
    arg_vals[i] = ME_RLI_IR_expr_eval(arg_curr->expr);
    arg_curr = arg_curr->next;
  }
  //resolve function name and evaluate
  ME_RLI_API_func f = ME_RLI_API_func_look_up(func->func_name->value);  
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
