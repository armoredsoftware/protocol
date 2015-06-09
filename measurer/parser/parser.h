struct ME_parser_token;

enum ME_parser_exper_type;

enum ME_parser_value_type;
struct ME_parser_value;
struct ME_parser_arg;
struct ME_parser_func;
struct ME_parser_expr;
struct ME_parser_sym;

extern struct ME_parser_expr * ME_parser_expr_create_value(struct ME_parser_value *);
extern struct ME_parser_expr * ME_parser_expr_create_func(struct ME_parser_func *);
extern void ME_parser_expr_print(struct ME_parser_expr *);
extern struct ME_parser_expr * ME_parser_expr_parse(struct ME_parser_token **);

extern struct ME_parser_func * ME_parser_func_create(struct ME_parser_sym *);
extern void ME_parser_func_print(struct ME_parser_func *);
extern struct ME_parser_func * ME_parser_func_parse(struct ME_parser_token **);
extern struct ME_parser_value ME_parser_func_eval(struct ME_parser_func *);

extern struct ME_parser_sym * ME_parser_sym_create(char *);
extern void ME_parser_sym_print(struct ME_parser_sym *);
extern struct ME_parser_sym * ME_parser_sym_parse(struct ME_parser_token **);
