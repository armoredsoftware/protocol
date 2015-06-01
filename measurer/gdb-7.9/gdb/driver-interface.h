#include <stdbool.h>

struct BE_Context;

extern void BE_start_session(struct BE_Context *);

extern struct BE_Context * BE_context_create(void);
extern void BE_context_print(struct BE_Context *);
extern void BE_get_request(struct BE_Context *);
extern bool startsWith(const char *, const char *);

extern void BE_update_callgraph(struct BE_Context * bec);

extern void BE_rhandler_dispatch(struct BE_Context *, const char *);
extern void BE_rhandler_CG_begin(struct BE_Context *);
extern void BE_rhandler_CG_end(struct BE_Context *);
extern void BE_rhandler_CG_get(struct BE_Context *);
extern void BE_rhandler_callstack_get(struct BE_Context *);
extern void BE_rhandler_callstack_get_at(struct BE_Context *, const char *);
extern void BE_rhandler_print(struct BE_Context *);
extern void BE_rhandler_set_target(struct BE_Context *, char *);
extern void BE_rhandler_quit(struct BE_Context *);
