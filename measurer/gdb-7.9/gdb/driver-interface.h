#include <stdbool.h>

enum BE_feature_type;
struct BE_feature;

enum BE_event_type;
struct BE_event_b;
struct BE_event_t;
union BE_event_union;
struct BE_event;

struct BE_Context;

extern struct BE_event * BE_event_t_create(int, int);
extern struct BE_event * BE_event_b_create(int, char *, int, int);
//extern void BE_event_set_feature(struct BE_event *, enum BE_feature_type, char *);
//extern void BE_event_add_measurement(struct BE_event *, struct ME_measurement *);
extern void BE_event_print(struct BE_event *);
extern void BE_event_disable(struct BE_event *);
extern void BE_event_enable(struct BE_event *);
extern void BE_event_kill(struct BE_event *);

extern struct BE_event * BE_event_table_create();
extern void BE_event_table_add(struct BE_event *, struct BE_event *);
extern struct BE_event * BE_event_table_get(struct BE_event *, int);
  
extern void BE_start_session(struct BE_Context *);

extern struct BE_Context * BE_context_create(void);
extern void BE_context_print(struct BE_Context *);
extern void BE_get_request(struct BE_Context *);
extern bool startsWith(const char *, const char *);

extern void BE_update_callgraph(struct BE_Context *);

extern void BE_rhandler_dispatch(struct BE_Context *, const char *);
extern void BE_rhandler_CG_begin(struct BE_Context *);
extern void BE_rhandler_CG_end(struct BE_Context *);
extern void BE_rhandler_CG_get(struct BE_Context *);
extern void BE_rhandler_callstack_get(struct BE_Context *);
extern void BE_rhandler_callstack_get_at(struct BE_Context *, const char *);
extern void BE_rhandler_print(struct BE_Context *);
extern void BE_rhandler_set_target(struct BE_Context *, char *);
extern void BE_rhandler_quit(struct BE_Context *);


extern void BE_event_handle(struct BE_Context *, struct BE_event *);
extern void BE_event_table_handle(struct BE_Context *, struct BE_event *, int, char *, int);
