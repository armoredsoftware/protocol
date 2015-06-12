#define ME_STORE_SIZE 64
#include <stdbool.h>

enum BE_feature_type;
struct BE_feature;

typedef enum {BE_FEATURE_CALLSTACK, BE_FEATURE_VARIABLE} BE_feature_type;

typedef struct BE_feature {
  BE_feature_type type;
  char var_name[64]; //TODO max length of var_name
} BE_feature;

enum BE_hook_type;
struct BE_hook_b;
struct BE_hook_t;
union BE_hook_union;
struct BE_hook;

typedef enum {BE_EVENT_T, BE_EVENT_B} BE_event_type;

typedef struct BE_event_t {
  int delay; //int time when
  clock_t start; //int time start
}
  BE_event_t;

typedef struct BE_event_b {
  int bp_id;
  char * filename; //TODO convert to array
  int line;
}
  BE_event_b;

typedef struct BE_event {
  BE_event_type type;
  bool repeat;

  union edata {
    struct BE_event_t t;
    struct BE_event_b b;
  } edata;
} BE_event;


struct BE_hook_array;
typedef struct BE_hook_array {
  struct BE_hook * hooks[64]; //MAX hook array size???
  int count;
}
BE_hook_array;


struct BE_Context;
typedef struct BE_Context
{
  bool attached;
  bool stopped;
  char * PID;
  int driverfd;
  bool CG_tracking;
  clock_t CG_lasttime;
  struct ME_CG * CG;
  struct ME_FT * FT;

  struct BE_hook_array hook_array;

  //measurement store
  struct ME_measurement * store[ME_STORE_SIZE]; 
  
}
BE_Context;

BE_Context the_context;

extern struct BE_event * BE_event_t_create(int, int);
extern struct BE_event * BE_event_b_create(int, char *, int, int);
//extern void BE_hook_set_feature(struct BE_hook *, enum BE_feature_type, char *);
//extern void BE_hook_add_measurement(struct BE_hook *, struct ME_measurement *);
//
extern void BE_hook_print(struct BE_hook *);
extern void BE_hook_disable(struct BE_hook *);
extern void BE_hook_enable(struct BE_hook *);
extern void BE_hook_kill(struct BE_hook *);

//extern struct BE_hook * BE_hook_table_create();
extern int BE_hook_array_add(struct BE_hook *);
extern struct BE_hook * BE_hook_array_get(int);
  
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


extern void BE_hook_handle(struct BE_Context *, struct BE_hook *);
extern void BE_hook_table_handle(struct BE_Context *, struct BE_hook *, int, char *, int);

extern struct ME_measurement * ME_API_measure_callstack();
extern struct ME_measurement * ME_API_measure(BE_feature *);
extern void ME_API_store(int, struct ME_measurement *);
extern struct ME_measurement * ME_API_load(int);
extern struct BE_event * ME_API_delay(int, int);
extern struct BE_event * ME_API_reach(char *, int,int);
extern struct BE_feature * ME_API_callstack();
extern struct BE_feature * ME_API_var(char *);

