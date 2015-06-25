#include "defs.h"
#include "driver-interface.h"
#include "ME_common.c"
#include "ME_RLI_IR.c"
#include "ME_RLI_IR_API.c"
#include "queue.h"
#include "ME_gdb.c"

//GDB includes
#include "top.h"
//#include "infrun.c"

#ifdef HAVE_POLL
#if defined (HAVE_POLL_H)
#include <poll.h>
#elif defined (HAVE_SYS_POLL_H)
#include <sys/poll.h>
#endif
#endif

#include <sys/types.h>
#include <sys/time.h>
#include "gdb_select.h"
#include "observer.h"

#include <stdbool.h>

#include <assert.h>

#include <jansson.h>

void BE_hook_disable(BE_hook * hook) {
  if (!hook) return;
  hook->enabled = false;
}

void BE_hook_enable(BE_hook * hook) {
  if (!hook) return;
  hook->enabled = true;
}

void BE_hook_kill(BE_hook * hook) {
  if (!hook) return;

  BE_hook_disable(hook);
  
  if (hook->event->type == BE_EVENT_B) {
    //remove breakpoint
    //TODO What happens if there is more than one event for this breakpoint??
    
    //interrupt and delete breakpoint
    execute_command("interrupt",0);
    wait_for_inferior(); 
    normal_stop();

    char arg[15];
    sprintf(arg, "%d", hook->event->edata.b.bp_id);
    delete_command(arg,0);

    //continue
    continue_command_JG();
  }
}

void BE_hook_array_init()
{
  the_context.hook_array.count = 0;
}

int BE_hook_array_add(BE_hook * hook) {
  //TODO check for bounds
  int i = the_context.hook_array.count++;
  the_context.hook_array.hooks[i] = hook;
  return i;
}

BE_hook * BE_hook_array_get(int i) {
  return the_context.hook_array.hooks[i];
}

/*====================================================
  CONTEXT STUFF
  ====================================================*/

/*typedef struct BE_Context
{
  bool attached;
  bool stopped;
  char * PID;
  int driverfd;
  bool CG_tracking;
  clock_t CG_lasttime;
  ME_CG * CG;
  ME_FT * FT;

  BE_hook * et;
}
BE_Context;*/

void BE_start_session()
{
  if (the_context.driverfd!=-1)
  {
    printf("Session already started!\n");
    exit(-1);
  }
  the_context.driverfd = ME_sock_server_connect();
}

BE_Context * BE_context_create(void)
{
  the_context.attached = false;
  the_context.stopped = false;
  the_context.PID = -1;
  the_context.driverfd = -1;
  the_context.CG_tracking = false;
  the_context.CG_lasttime = 0;
  the_context.CG = NULL;
  the_context.FT = ME_FT_create("root");
  
  return &the_context;
  
}

void BE_context_print()
{
  printf("Measurer Context {PID=%d,CG_tracking=%d,CG_lasttime=%ju,",the_context.PID,the_context.CG_tracking,(uintmax_t)the_context.CG_lasttime);
  printf("\nCG=");
  ME_CG_print(the_context.CG,the_context.FT);
  printf(",\nFT=");
  ME_FT_print(the_context.FT);
  printf("}\n");
}

bool startsWith(const char *pre, const char *str)
{
  size_t lenpre = strlen(pre),
    lenstr = strlen(str);
  return lenstr < lenpre ? false : strncmp(pre, str, lenpre) == 0;
}

void BE_get_request()
{
 
  char request[1024];

  int n = ME_sock_recv(the_context.driverfd, request);

  if (n <= 0 || !(*request))
    return;
  
  //Parse out JSON
  json_t *root, *params;
  json_error_t error;
  root = json_loads(request, 0, &error);
  params = json_object_get(root,"params");
  char * RLI_expr = json_string_value(params);
  
  ME_RLI_IR_value value_result= BE_rhandler_dispatch(RLI_expr);

  json_decref(root);

  //Send response
  root = json_object();
  json_object_set_new(root, "jsonrpc", json_string("2.0"));

  if (value_result.type == ME_RLI_IR_VALUE_MEASUREMENT) {
    json_object_set_new(root, "result", ME_measurement_toJSON(value_result.vdata.ms));
  }
  else {
    json_object_set_new(root, "result", json_null());
  }


  json_object_set_new(root, "id", json_string("1"));

  char * response;
  response = json_dumps( root, 0 );
  
  ME_sock_send(the_context.driverfd, response);
  
  json_decref(root);  

}

void BE_update_callgraph()
{
  
  printf("\nUpdate Call Graph!\n");
  execute_command("interrupt",0);
  wait_for_inferior(); 
  normal_stop();
  struct ME_CG * stack;
  BE_get_call_stack_as_CG(NULL, 0, 0, 1, &stack, the_context.FT);
  continue_command_JG();
  
  if (!the_context.CG)
  {
    the_context.CG = stack;
  }
  else
  {
    ME_CG_merge_stack(the_context.CG,stack);
    ME_CG_delete(stack);
  }
  ME_CG_print(the_context.CG,the_context.FT);
  //execute_command("detach",1);

}


void BE_do_continuous()
{
  
  if (!the_context.attached) {
    return;
  }

  //Check inferior for stops
  char * filename = NULL;
  int line = -1;
  int bp_id;
  int breaked = fetch_inferior_event_JG(&filename, &line, &bp_id);
  if (breaked) {
    the_context.stopped = true;
    printf("Stop caught at %s:%d !\n", filename, line);
  }

  //Check events
  BE_hook_array_handle(breaked, filename, line, bp_id);

  if (the_context.stopped) {
    continue_command_JG();
    the_context.stopped = false;
  }
    
  clock_t t = clock();
  
  //Update Call Graph
  if (the_context.CG_tracking && t >= the_context.CG_lasttime + 10000000)
  {
    BE_update_callgraph();
    the_context.CG_lasttime = t;
  }
  
}

struct ME_RLI_IR_value BE_rhandler_dispatch(char * request)
{  
  printf("\nHandling %s request!\n",request);
  
  ME_RLI_token * tokens = ME_RLI_tokenize(request);
  if (!tokens) return;
    
  ME_RLI_IR_expr * expr = ME_RLI_IR_expr_parse(&tokens);
  if (!expr) return;
  //ME_RLI_IR_expr_print(expr);
  //printf("\n");
  
  ME_RLI_IR_value result = ME_RLI_IR_expr_eval(expr);
    
  printf("result = ");
  ME_RLI_IR_value_print(result);
  printf("\n");
    
  //TODO delete tokens, expr
    
  //printf("\nUnrecognized request! request=%s\n",request);

  return result;
}

/*============================================================*/
void BE_hook_array_handle(int breaked, char * filename, int line, int bp_id)
{  
  //check event_t
  int i = 0;
  for (i=0; i<the_context.hook_array.count;i++) {
    BE_hook * curr = BE_hook_array_get(i);  
    clock_t t = clock();
    if (curr->enabled) {
      switch (curr->event->type) {
      case BE_EVENT_T:
	if (t >= curr->event->edata.t.start + curr->event->edata.t.delay) {

	  //stop if not stopped
	  if (!the_context.stopped) {
	    //Abstract this away
	    execute_command("interrupt",0);
	    wait_for_inferior(); 
	    normal_stop();

	    BE_get_file_and_line(get_selected_frame(NULL), &filename, &line); //put these in the BEC

	    the_context.stopped = true;
	    //continue_command_JG();
	  }    

	  
	  BE_hook_handle(curr);
	  
	  if (curr->event->repeat) {
	    curr->event->edata.t.start = t;
	  } else {
	    curr->enabled = false;
	  }
	  
	}
	break;
      case BE_EVENT_B:
	break;
      }
    }
  }

  //check event_b
  for (i=0; i<the_context.hook_array.count;i++) {
    BE_hook * curr = BE_hook_array_get(i);  
    if (curr->enabled) {
      switch (curr->event->type) {
      case BE_EVENT_T:
	break;
      case BE_EVENT_B:
	if (breaked) {
	  if (bp_id == curr->event->edata.b.bp_id)
	  {
	    printf("At the breakpoint for this event!\n");
	    BE_hook_handle(curr);
	    printf("Event handled\n");
	    if (!curr->event->repeat) {
	      char arg[15];
	      sprintf(arg, "%d", curr->event->edata.b.bp_id);
	      delete_command(arg,0);

	      curr->enabled = 0;
	    }
	    
	  }
	}
      }
      
    }
  }  
}

void BE_hook_handle(BE_hook * hook) {
  //printf("handling e%d! delay = %d, start = %d\n", i, ev->delay, ev->start);
  if (hook->action) {
    ME_RLI_IR_expr_eval(hook->action);
    return;
  }
}

void BE_ehandler_print()
{
  BE_context_print();
}

void BE_ehandler_measure_callstack(BE_hook * ev)
{  
  if (!the_context.attached || !the_context.stopped) {
    printf("Not attached to a process!\n");
    return;
  }
  
  struct ME_CG * stack;
  struct ME_FT * ft = ME_FT_create("root");
  printf("before\n");
  BE_get_call_stack_as_CG(NULL, 0, 0, 1, &stack, ft);
  printf("after\n");

  ME_measurement * ms = ME_measurement_create(ME_MEASUREMENT_CALLSTACK);
  ms->data.cgft.cg = stack;
  ms->data.cgft.ft = ft;

  //BE_hook_add_measurement(ev, ms);
  
}

void BE_ehandler_measure_variable(BE_hook * ev)
{
  if (!the_context.attached || !the_context.stopped) {
    printf("Not attached to a process!\n");
    return;
  }

  /*char * value = BE_get_variable(ev->feature.expr, 0);
  printf("Value of %s = %s\n", ev->feature.expr, value);
    
  ME_measurement * ms = ME_measurement_create(ME_MEASUREMENT_STRING);
  ms->data.string_val = value;

  BE_hook_add_measurement(ev, ms);
  */
}

/*====================================================
  API STUFF
  ====================================================*/

void ME_API_set_target(int target_PID)
{
  printf("ME_API_set_target(%d)\n",target_PID);
  
  if (the_context.attached) {
    ME_API_detach();
  }
  
  the_context.PID = target_PID;
  
  //Attach to process

  char PID_str[64];
  sprintf(PID_str, "%d", target_PID);  
  attach_command(PID_str,1);
  gdb_do_one_event ();

  the_context.attached = true;
  
  continue_command_JG();
}

void ME_API_detach()
{
  if (the_context.attached) {
    //Detach
    execute_command("detach",0);

    //Update context
    the_context.attached = false;
    the_context.PID = -1;
  }
}

void ME_API_quit()
{
  if (the_context.attached) {
    ME_API_detach();
  }
    
  exit(-1);
}

void ME_API_print_context()
{
  BE_context_print(&the_context);
}

ME_measurement * ME_API_measure_callstack()
{
  ME_measurement * ms = NULL;
  
  if (!the_context.attached) {
    printf("Not attached to a process!\n");
    return;
  }

  bool stopped_here = false;
  if (!the_context.stopped) {
    //Interrupt Inferior
    execute_command("interrupt",0);
    wait_for_inferior(); 
    normal_stop();
    stopped_here = true;
  }
 
  //Measure Callstack
  printf("getting callstack\n");
  struct ME_CG * stack;
  struct ME_FT * ft = ME_FT_create("root");
  printf("before\n");
  BE_get_call_stack_as_CG(NULL, 0, 0, 1, &stack, ft);
  printf("after\n");

  if (stopped_here) {
    //Continue Inferior
    continue_command_JG();
  }
    
  //Create and return measurement
  ms = ME_measurement_create(ME_MEASUREMENT_CALLSTACK);
  ms->data.cgft.cg = stack;
  ms->data.cgft.ft = ft;

  return ms;
}

ME_measurement * ME_API_measure(ME_feature * feature)
{
  ME_measurement * ms;
  
  if (!the_context.attached) {
    printf("Not attached to a process!\n");
    return NULL;
  }

  bool stopped_here = false;
  if (!the_context.stopped) {
    //Interrupt Inferior
    execute_command("interrupt",0);
    wait_for_inferior(); 
    normal_stop();
    stopped_here = true;
  }
 
  //Measure Callstack
  if (feature->type == ME_FEATURE_CALLSTACK) {
    printf("getting callstack\n");
    struct ME_CG * stack;
    struct ME_FT * ft = ME_FT_create("root");
    printf("before\n");
    BE_get_call_stack_as_CG(NULL, 0, 0, 1, &stack, ft);
    printf("after\n");

    ms = ME_measurement_create(ME_MEASUREMENT_CALLSTACK);
    ms->data.cgft.cg = stack;
    ms->data.cgft.ft = ft;
  } else if (feature->type == ME_FEATURE_VARIABLE) {
    printf("getting var\n");
    char * value = BE_get_variable(feature->fdata.var_name, 0);
    
    ms = ME_measurement_create(ME_MEASUREMENT_STRING);
    ms->data.string_val = value;
  } else if (feature->type == ME_FEATURE_MEMORY) {
    printf("getting mem\n");
    char * value = BE_get_memory(feature->fdata.m.address, feature->fdata.m.format);
    
    ms = ME_measurement_create(ME_MEASUREMENT_STRING);
    ms->data.string_val = value;
  }
    
  if (stopped_here) {
    //Continue Inferior
    continue_command_JG();
  }

  return ms;
}

void ME_API_sendme(ME_measurement * ms)
{  
  ME_measurement_print(ms);
  ME_measurement_send_temp(the_context.driverfd, ms);
  printf("Sent measurements!\n");
}

void ME_API_store(int i, ME_measurement * ms) {
  if (i >= ME_STORE_SIZE) {
    printf("Could not store beyond bounds of measurement store!\n");
  }
  ms->next = the_context.store[i];
  the_context.store[i] = ms;
}

ME_measurement * ME_API_load(int i) {
  if (i >= ME_STORE_SIZE) {
    printf("Could not load beyond bounds of measurement store!\n");
    return NULL;
  }
  return the_context.store[i];
}

BE_event * ME_API_delay(int delay, int repeat) {
  return BE_event_t_create(delay,repeat);
}

BE_event * ME_API_reach(char * filename, int line, int repeat) {
  BE_event * event = BE_event_b_create(get_breakpoint_count()+1,repeat);

  //interrupt and insert breakpoint
  execute_command("interrupt",0);
  wait_for_inferior(); 
  normal_stop();
  char arg[64];
  sprintf(arg, "%s:%d", filename, line);
  break_command(arg,0);

  //continue
  continue_command_JG();
      
  return event;
}

BE_event * ME_API_reach_func(char * func_name, int repeat) {
  BE_event * event = BE_event_b_create(get_breakpoint_count()+1,repeat);

  //interrupt and insert breakpoint
  execute_command("interrupt",0);
  wait_for_inferior(); 
  normal_stop();
  break_command(func_name,0);

  //continue
  continue_command_JG();
      
  return event;
}

int ME_API_hook(struct BE_event * event, struct ME_RLI_IR_expr * action) {
  if (!the_context.attached) {
    return -1;
  }

  BE_hook * hook = (BE_hook *)malloc(sizeof(BE_hook));
  hook->event = event;
  hook->action = action;
  hook->enabled = true;
  int i = BE_hook_array_add(hook);  
  return i;
}

void ME_API_kill(int i) {
  BE_hook_kill(BE_hook_array_get(i));
} 

void ME_API_enable(int i) {
  BE_hook_enable(BE_hook_array_get(i));
} 

void ME_API_disable(int i) {
  BE_hook_disable(BE_hook_array_get(i));
} 

ME_feature * ME_API_callstack() {
  return ME_feature_create_callstack();
}

ME_feature * ME_API_var(char * var_name) {
  return ME_feature_create_variable(var_name);
}

ME_feature * ME_API_mem(char * address, char * format) {
  return ME_feature_create_memory(address, format);
}

void ME_API_gdb(char * command) {
  return execute_command(command, 1);
}
