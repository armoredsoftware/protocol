#include "ME_RLI_IR.h"

typedef struct ME_RLI_IR_value (*ME_RLI_API_func)(struct ME_RLI_IR_value *, int);

extern struct ME_RLI_IR_value ME_RLI_IR_API_strlen(struct ME_RLI_IR_value *, int);
extern struct ME_RLI_IR_value ME_RLI_IR_API_print(struct ME_RLI_IR_value *, int);
extern struct ME_RLI_IR_value ME_RLI_IR_API_eq(struct ME_RLI_IR_value *, int);
extern struct ME_RLI_IR_value ME_RLI_IR_API_if(struct ME_RLI_IR_value *, int);
extern struct ME_RLI_IR_value ME_RLI_IR_API_seq(struct ME_RLI_IR_value *, int);
extern struct ME_RLI_IR_value ME_RLI_IR_API_set_target(struct ME_RLI_IR_value *, int);
extern struct ME_RLI_IR_value ME_RLI_IR_API_detach(struct ME_RLI_IR_value *, int);
extern struct ME_RLI_IR_value ME_RLI_IR_API_quit(struct ME_RLI_IR_value *, int);
extern struct ME_RLI_IR_value ME_RLI_IR_API_print_context(struct ME_RLI_IR_value *, int);
extern struct ME_RLI_IR_value ME_RLI_IR_API_measure_callstack(struct ME_RLI_IR_value *, int);
extern struct ME_RLI_IR_value ME_RLI_IR_API_measure(struct ME_RLI_IR_value *, int);
extern struct ME_RLI_IR_value ME_RLI_IR_API_sendme(struct ME_RLI_IR_value *, int);
extern struct ME_RLI_IR_value ME_RLI_IR_API_store(struct ME_RLI_IR_value *, int);
extern struct ME_RLI_IR_value ME_RLI_IR_API_load(struct ME_RLI_IR_value *, int);
extern struct ME_RLI_IR_value ME_RLI_IR_API_delay(struct ME_RLI_IR_value *, int);
extern struct ME_RLI_IR_value ME_RLI_IR_API_reach(struct ME_RLI_IR_value *, int);
extern struct ME_RLI_IR_value ME_RLI_IR_API_reach_func(struct ME_RLI_IR_value *, int);
extern struct ME_RLI_IR_value ME_RLI_IR_API_hook(struct ME_RLI_IR_value *, int);
extern struct ME_RLI_IR_value ME_RLI_IR_API_callstack(struct ME_RLI_IR_value *, int);
extern struct ME_RLI_IR_value ME_RLI_IR_API_var(struct ME_RLI_IR_value *, int);
extern struct ME_RLI_IR_value ME_RLI_IR_API_mem(struct ME_RLI_IR_value *, int);
extern struct ME_RLI_IR_value ME_RLI_IR_API_kill(struct ME_RLI_IR_value *, int);
extern struct ME_RLI_IR_value ME_RLI_IR_API_enable(struct ME_RLI_IR_value *, int);
extern struct ME_RLI_IR_value ME_RLI_IR_API_disable(struct ME_RLI_IR_value *, int);
extern struct ME_RLI_IR_value ME_RLI_IR_API_gdb(struct ME_RLI_IR_value *, int);

extern struct ME_RLI_IR_value ME_RLI_IR_API_check_args(enum ME_RLI_IR_value_type *, int, struct ME_RLI_IR_value *, int);
extern ME_RLI_API_func ME_RLI_API_func_look_up(char *);

