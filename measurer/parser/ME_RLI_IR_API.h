#include "ME_RLI_IR.h"

typedef struct ME_RLI_IR_value (*ME_RLI_API_func)(struct ME_RLI_IR_value *, int);

extern struct ME_RLI_IR_value ME_RLI_IR_API_add(struct ME_RLI_IR_value *, int);
extern struct ME_RLI_IR_value ME_RLI_IR_API_subtract(struct ME_RLI_IR_value *, int);
extern struct ME_RLI_IR_value ME_RLI_IR_API_strlen(struct ME_RLI_IR_value *, int);
extern struct ME_RLI_IR_value ME_RLI_IR_API_print(struct ME_RLI_IR_value *, int);
extern struct ME_RLI_IR_value ME_RLI_IR_API_eq(struct ME_RLI_IR_value *, int);
extern struct ME_RLI_IR_value ME_RLI_IR_API_if(struct ME_RLI_IR_value *, int);
extern struct ME_RLI_IR_value ME_RLI_IR_API_seq(struct ME_RLI_IR_value *, int);

extern ME_RLI_API_func ME_RLI_API_func_look_up(char *);
