#include<stdio.h>
#include<stdlib.h>
#include<string.h>

#include "parser.h"

typedef struct ME_parser_token {
  char value[64]; //max token length??? make constant
  struct ME_parser_token * next;
} ME_parser_token;

ME_parser_token * ME_parser_token_create(char * value) {
  ME_parser_token * token = (ME_parser_token*)malloc(sizeof(ME_parser_token));
  strcpy(token->value, value);
  return token;
}

void ME_parser_token_print(ME_parser_token * token) {
  if (!token) return;

  printf("%s", token->value);

  if (token->next) {
    printf(",");
    ME_parser_token_print(token->next);
  }
}

ME_parser_token * ME_parser_tokenize(char * in) {
  int curr = 0;
  char token[64]; //maximum token length???...
  char token_curr = 0;

  ME_parser_token * tokens = NULL;
  ME_parser_token * tokens_curr = NULL;
  
  while (1) {
     
    //consume token
    if (in[curr] == '(' || in[curr] == ')') {
      token[token_curr++] = in[curr++];
    } else if (in[curr] == '"') {
      token[token_curr++] = in[curr++]; 
      while (in[curr] != '"') {
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
      tokens_curr->next = ME_parser_token_create(token);
      tokens_curr = tokens_curr->next;
    } else {
      tokens = ME_parser_token_create(token);
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

typedef enum ME_parser_value_type {
  ME_PARSER_VALUE_INT, ME_PARSER_VALUE_STRING, ME_PARSER_VALUE_VOID, ME_PARSER_VALUE_LEXPR
}
ME_parser_value_type;

typedef struct ME_parser_value {
  ME_parser_value_type type; 
  union vdata {
    int int_val;
    char string_val[64]; //max string length...
    struct ME_parser_expr * lexpr;
  } vdata;
}
ME_parser_value;

typedef enum ME_parser_expr_type {
  ME_PARSER_EXPR_VALUE, ME_PARSER_EXPR_FUNC
}
ME_parser_expr_type;

typedef struct ME_parser_expr {
  ME_parser_expr_type type;
  union data {
    struct ME_parser_value * value;
    struct ME_parser_func * func;
  } data;
  
}
ME_parser_expr;

typedef struct ME_parser_arg {
  ME_parser_expr * expr;
  struct ME_parser_arg * next;
}
  ME_parser_arg;

typedef struct ME_parser_func {
  struct ME_parser_sym * func_name;
  struct ME_parser_arg * args;    
}
ME_parser_func;

typedef struct ME_parser_sym {
  char value[32]; //max size?
}
ME_parser_sym;

// VALUE STUFF

ME_parser_value ME_parser_value_create_int(int int_val) {
  struct ME_parser_value value;
  value.type = ME_PARSER_VALUE_INT;
  value.vdata.int_val = int_val;
  return value;
}

ME_parser_value ME_parser_value_create_string(char * string_val) {
  struct ME_parser_value value;
  value.type = ME_PARSER_VALUE_STRING;
  strcpy(value.vdata.string_val, string_val);
  return value;
}

ME_parser_value ME_parser_value_create_void() {
  struct ME_parser_value value;
  value.type = ME_PARSER_VALUE_VOID;
  return value;
}

ME_parser_value ME_parser_value_create_lexpr(ME_parser_expr * lexpr) {
  struct ME_parser_value value;
  value.type = ME_PARSER_VALUE_LEXPR;
  value.vdata.lexpr = lexpr;
  return value;
}

void ME_parser_value_print(ME_parser_value value) {
  if (value.type == ME_PARSER_VALUE_INT) {
    printf("INT:%d",value.vdata.int_val);
  } else if (value.type == ME_PARSER_VALUE_STRING) {
    printf("STRING:\"%s\"",value.vdata.string_val);
  } else if (value.type == ME_PARSER_VALUE_VOID) {
    printf("VOID");
  } else if (value.type == ME_PARSER_VALUE_LEXPR) {
    printf("LEXPR:");
    ME_parser_expr_print(value.vdata.lexpr);
  }
}

ME_parser_value * ME_parser_value_parse(ME_parser_token ** curr) {
  ME_parser_value * value = (ME_parser_value*)malloc(sizeof(ME_parser_value));

  //if string pattern detected
  if (strcmp((*curr)->value,"'")==0) {
    //consume first tick
    (*curr) = (*curr)->next;

    ME_parser_expr * lexpr = ME_parser_expr_parse(curr);
    (*value) = ME_parser_value_create_lexpr(lexpr);
    
  } else if ((*curr)->value[0] == '"') {
    char temp[strlen((*curr)->value)-1];
    strcpy(temp,(*curr)->value+1);
    temp[strlen(temp)-1] = 0;
    
    (*value) = ME_parser_value_create_string(temp);
    (*curr) = (*curr)->next;
  } else {

    int int_val = atoi((*curr)->value);
    (*curr) = (*curr)->next;
    (*value) = ME_parser_value_create_int(int_val);

  }
  
  return value;
}

// EXPR STUFF
ME_parser_expr * ME_parser_expr_create_value(ME_parser_value * value) {
  ME_parser_expr * expr = (ME_parser_expr*)(malloc(sizeof(ME_parser_expr)));
  expr->type = ME_PARSER_EXPR_VALUE;
  expr->data.value = value;
  return expr;
}

ME_parser_expr * ME_parser_expr_create_func(ME_parser_func * func) {
  ME_parser_expr * expr = (ME_parser_expr*)(malloc(sizeof(ME_parser_expr)));
  expr->type = ME_PARSER_EXPR_FUNC;
  expr->data.func = func;
  return expr;
}

void ME_parser_expr_print(ME_parser_expr * expr)
{
  if (expr->type == ME_PARSER_EXPR_VALUE) {
    ME_parser_value_print((*expr->data.value));
  }
  else if (expr->type == ME_PARSER_EXPR_FUNC) {
    ME_parser_func_print(expr->data.func);
  }
}

ME_parser_expr * ME_parser_expr_parse(ME_parser_token ** curr) {

  ME_parser_expr * expr = NULL;
  
  if (strcmp((*curr)->value,"(")==0) {
    expr = ME_parser_expr_create_func(ME_parser_func_parse(curr));
  } else {
    expr = ME_parser_expr_create_value(ME_parser_value_parse(curr));
  }
}

ME_parser_value ME_parser_expr_eval(ME_parser_expr * expr) {
  if (expr->type == ME_PARSER_EXPR_VALUE) {
    return (*expr->data.value);
  }
  else if (expr->type == ME_PARSER_EXPR_FUNC) {
    return ME_parser_func_eval(expr->data.func);
    
  }
}

// FUNC STUFF
ME_parser_func * ME_parser_func_create(ME_parser_sym * func_name) {
  ME_parser_func * func = (ME_parser_func*)(malloc(sizeof(ME_parser_func)));
  func->func_name = func_name;
  return func;
}

void ME_parser_func_print(ME_parser_func * func)
{
  printf("(");
  ME_parser_sym_print(func->func_name);

  ME_parser_arg * arg_curr = func->args;

  while (arg_curr) {
    printf(" ");
    ME_parser_expr_print(arg_curr->expr);
    arg_curr = arg_curr->next;
  }
  
  printf(")");
}

int ME_parser_func_arg_count(ME_parser_func * func)
{
  ME_parser_arg * arg_curr = func->args;
  int count = 0;
  
  while (arg_curr) {
    arg_curr = arg_curr->next;
    count++;
  }

  return count;
}

void ME_parser_func_add_arg(ME_parser_func * func, ME_parser_expr * expr) {
  ME_parser_arg * arg = (ME_parser_arg*)malloc(sizeof(ME_parser_arg));
  arg->expr = expr;

  if (!func->args) {
    func->args = arg;
    return;
  }
  
  ME_parser_arg * curr = func->args;
  while(curr->next) {
    curr = curr->next;
  }

  curr->next = arg;
}

ME_parser_func * ME_parser_func_parse(ME_parser_token ** curr) {
  //consume parenthesis
  if (strcmp((*curr)->value,"(")!=0) {
    printf("Parse error: expected \"(\"");
    exit(-1);
  }
  (*curr) = (*curr)->next;

  //consume function name
  ME_parser_sym * func_name = ME_parser_sym_parse(curr);

  ME_parser_func * func = ME_parser_func_create(func_name);

  //consume arguments
  while (strcmp((*curr)->value,")")!=0) {
    //parse expression
    ME_parser_expr * arg = ME_parser_expr_parse(curr);

    ME_parser_func_add_arg(func, arg);
  }

  //consume parenthesis
  (*curr) = (*curr)->next;
  
  return func;
}

int add(int a, int b) {
  return a + b;
}
ME_parser_value ME_API_add(ME_parser_value * arg_vals, int arg_count) {
  return ME_parser_value_create_int(add(arg_vals[0].vdata.int_val,arg_vals[1].vdata.int_val));  
}
int subtract(int a, int b) {
  return a - b;
}
ME_parser_value ME_API_subtract(ME_parser_value * arg_vals, int arg_count) {
  return ME_parser_value_create_int(subtract(arg_vals[0].vdata.int_val,arg_vals[1].vdata.int_val));  
}
ME_parser_value ME_API_strlen(ME_parser_value * arg_vals, int arg_count) {
  return ME_parser_value_create_int(strlen(arg_vals[0].vdata.string_val));  
}
ME_parser_value ME_API_print(ME_parser_value * arg_vals, int arg_count) {
  printf("\nPRINT %s\n",arg_vals[0].vdata.string_val);
  return ME_parser_value_create_void();
}
ME_parser_value ME_API_eq(ME_parser_value * arg_vals, int arg_count) {
  if (arg_count != 2) {
    printf("eq needs exactly 2 arguments!\n");
    exit(-1);
  }
  
  if (arg_vals[0].type != arg_vals[1].type)
    return ME_parser_value_create_int(0);
  if (arg_vals[0].type == ME_PARSER_VALUE_INT) {
    if (arg_vals[0].vdata.int_val == arg_vals[1].vdata.int_val)
      return ME_parser_value_create_int(1);
    else
      return ME_parser_value_create_int(0);
  } else {
    printf("eq not implemented for this type!\n");
    exit(-1);
  }
}
ME_parser_value ME_API_if(ME_parser_value * args_vals, int arg_count) {
  if (arg_count != 3) {
    printf("if needs exactly 3 arguments!\n");
    exit(-1);
  }
  if (args_vals[0].vdata.int_val)
    return args_vals[1];
  else
    return args_vals[2];
  /*if (args_vals[0].vdata.int_val)
    return ME_parser_expr_eval(args_vals[1].vdata.lexpr);
  else
  return ME_parser_expr_eval(args_vals[2].vdata.lexpr);*/
}

ME_parser_value ME_API_seq(ME_parser_value * args_vals, int arg_count) {
  int i;
  for (i = 0; i < arg_count; i++) {
    ME_parser_expr_eval(args_vals[i].vdata.lexpr);
  }
  return ME_parser_value_create_void();
}

typedef ME_parser_value (*ME_API_func)(ME_parser_value *, int);

ME_API_func ME_API_func_look_up(char * func_name) {
  if (strcmp(func_name,"add")==0) return &ME_API_add;
  else if (strcmp(func_name,"subtract")==0) return &ME_API_subtract;
  else if (strcmp(func_name,"strlen")==0) return &ME_API_strlen;
  else if (strcmp(func_name,"print")==0) return &ME_API_print;
  else if (strcmp(func_name,"seq")==0) return &ME_API_seq;
  else if (strcmp(func_name,"eq")==0) return &ME_API_eq;
  else if (strcmp(func_name,"if")==0) return &ME_API_if;
  else {
    printf("Interpreter: No such function \"%s\"!\n",func_name);
    exit(-1);
  }
}


ME_parser_value ME_parser_func_eval(ME_parser_func * func) {
  //evaluate arguments
  int args_count = ME_parser_func_arg_count(func);
  ME_parser_value arg_vals[args_count];
  int i = 0;
  ME_parser_arg * arg_curr = func->args;
  for (i = 0; i<args_count; i++) {
    arg_vals[i] = ME_parser_expr_eval(arg_curr->expr);
    arg_curr = arg_curr->next;
  }
  //resolve function name and evaluate
  ME_API_func f = ME_API_func_look_up(func->func_name->value);  
  ME_parser_value result;
  result = (*f)(arg_vals,args_count);
  return result;
}

// SYM STUFF
ME_parser_sym * ME_parser_sym_create(char * value) {
  ME_parser_sym * sym = (ME_parser_sym*)(malloc(sizeof(ME_parser_sym)));
  strcpy(sym->value, value);
  return sym;
}

void ME_parser_sym_print(ME_parser_sym * sym)
{
  printf("%s",sym->value);
}

ME_parser_sym * ME_parser_sym_parse(ME_parser_token ** curr) {
  ME_parser_sym * sym = ME_parser_sym_create((*curr)->value);
  (*curr) = (*curr)->next;
  return sym;
}

void main()
{
  //ME_parser_token * tokens = ME_parser_tokenize("(add (subtract 1 3) 1)");
  //ME_parser_token * tokens = ME_parser_tokenize("(seq '(print \"a\") '(print \"b\"))");
  //ME_parser_token * tokens = ME_parser_tokenize("(if (eq 1 1) '(print \"a\") 2 ) ");
  ME_parser_token * tokens = ME_parser_tokenize("(print \"a b c\")");
  ME_parser_token_print(tokens);

  printf("\n");

  ME_parser_expr * expr = ME_parser_expr_parse(&tokens);
  
  ME_parser_expr_print(expr);

  ME_parser_value result = ME_parser_expr_eval(expr);
  
  printf("\nValue = ");
  ME_parser_value_print(result);

  printf("\n");
  
  /*ME_parser_sym * sym1 = ME_parser_sym_create("temp");
  ME_parser_sym_print(sym1);
  printf("\n");

  ME_parser_expr * expr1 = ME_parser_expr_create_int(99);
  ME_parser_expr_print(expr1);
  printf("\n");

  ME_parser_sym * sym_add = ME_parser_sym_create("add");
  ME_parser_expr * expr_add1 = ME_parser_expr_create_int(3);
  ME_parser_expr * expr_add2 = ME_parser_expr_create_int(7);
  ME_parser_expr ** args_add = (ME_parser_expr**)malloc(sizeof(ME_parser_expr*)*2);
  args_add[0] = expr_add1;
  args_add[1] = expr_add2;
  ME_parser_func * func2 = ME_parser_func_create(sym_add, 2, args_add);

  ME_parser_expr * expr2 = ME_parser_expr_create_func(func2);
  ME_parser_expr_print(expr2);
  printf("\n");
  
  ME_parser_expr ** args = (ME_parser_expr**)malloc(sizeof(ME_parser_expr*)*2);
  args[0] = expr1;
  args[1] = expr2;
  
  ME_parser_func * func1 = ME_parser_func_create(sym1, 2, args);

  ME_parser_func_print(func1);
  printf("\n");*/


  
  
}
