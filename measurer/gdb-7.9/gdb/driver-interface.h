#define ME_STORE_SIZE 64
#include <stdbool.h>
#include "ME_RLI_IR.h"
#include "ME_common.h"

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
  int PID;
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

extern void BE_hook_array_init();
extern int BE_hook_array_add(struct BE_hook *);
extern struct BE_hook * BE_hook_array_get(int);
extern void BE_hook_array_handle(int, char *, int, int);
  
extern void BE_start_session();

extern struct BE_Context * BE_context_create(void);
extern void BE_context_print();
extern void BE_get_request();
extern bool startsWith(const char *, const char *);

extern void BE_update_callgraph();
extern void BE_do_continous();
extern struct ME_RLI_IR_value BE_rhandler_dispatch(char *);

extern void BE_hook_handle(struct BE_hook *);
extern void BE_hook_table_handle(struct BE_hook *, int, char *, int);

extern void ME_API_set_target(int);
extern void ME_API_detach();
extern void ME_API_quit();
extern void ME_API_print_context();
extern struct ME_measurement * ME_API_measure_callstack();
extern struct ME_measurement * ME_API_measure(struct ME_feature *);
extern void ME_API_sendme(struct ME_measurement *);
extern void ME_API_store(int, struct ME_measurement *);
extern struct ME_measurement * ME_API_load(int);
extern struct BE_event * ME_API_delay(int, int);
extern struct BE_event * ME_API_reach(char *, int,int);
extern struct BE_event * ME_API_reach_func(char *,int);
extern struct BE_event * ME_API_reach_syscall(char *,int);
extern int ME_API_hook(struct BE_event *, struct ME_RLI_IR_expr *);
extern void ME_API_kill(int);
extern void ME_API_enable(int);
extern void ME_API_disable(int);
extern struct ME_feature * ME_API_callstack();
extern struct ME_feature * ME_API_var(char *);
extern struct ME_feature * ME_API_mem(char *,char *);
extern void ME_API_gdb(char *);
