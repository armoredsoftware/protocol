#include <stdbool.h>

struct BE_CG;
struct BE_func_table;

extern struct BE_CG * BE_CG_create(int);
extern void BE_CG_add_child(struct BE_CG *, struct BE_CG *);
extern struct BE_CG * BE_CG_copy(struct BE_CG *);
extern void BE_CG_delete(struct BE_CG *);
extern void BE_CG_merge_stack(struct BE_CG *, struct BE_CG *);
extern void BE_CG_print_h(struct BE_CG *, struct BE_func_table *);
extern void BE_CG_print(struct BE_CG *, struct BE_func_table *);
extern int BE_CG_count(struct BE_CG *);
extern int BE_CG_encode_h(struct BE_CG *, int, int *);
extern void BE_CG_encode(struct BE_CG *, int *, int **);
extern void BE_CG_print_encoded(int, int *);
extern struct BE_CG * BE_CG_decode_h(int *, int);
extern struct BE_CG * BE_CG_decode(int *);

struct BE_Context;

extern struct BE_func_table * BE_func_table_create(char *);
extern int BE_func_table_add(struct BE_func_table *, char *);
extern void BE_func_table_print(struct BE_func_table *);
extern int BE_func_table_get_index(struct BE_func_table *, char *);
extern char * BE_func_table_get(struct BE_func_table *, int);

extern struct BE_Context* BE_create_context(void);
extern void BE_get_request(struct BE_Context *);
extern bool startsWith(const char *, const char *);

extern void BE_update_callgraph(struct BE_Context * bec);


extern void BE_rhandle_dispatch(struct BE_Context *, const char *);
extern void BE_rhandler_CG_begin(struct BE_Context *);
extern void BE_rhandler_CG_end(struct BE_Context *);
extern void BE_rhandler_CG_get(struct BE_Context *);
extern void BE_rhandler_print(struct BE_Context *);
extern void BE_rhandler_set_target(struct BE_Context *, char *);
extern void BE_rhandler_quit(struct BE_Context *);
