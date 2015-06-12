#include "defs.h"
#include "driver-interface.h"
#include "ME_common.c"
#include "ME_RLI_IR.c"
#include "ME_RLI_IR_API.c"
#include "queue.h"

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

/*====================================================
  FEATURE STUFF
  ====================================================*/

BE_feature * BE_feature_create_callstack() {
  BE_feature * feature = (BE_feature*)malloc(sizeof(BE_feature));
  feature->type = BE_FEATURE_CALLSTACK;
  return feature;
}

BE_feature * BE_feature_create_variable(char * var_name) {
  BE_feature * feature = (BE_feature*)malloc(sizeof(BE_feature));
  feature->type = BE_FEATURE_VARIABLE;
  strcpy(feature->var_name,var_name);//TODO enforce size limit 
  return feature;
}

void BE_feature_print(BE_feature * feature)
{
  if (feature->type == BE_FEATURE_CALLSTACK) {
    printf("{type=callstack}");
  } else {
    printf("{type=variable,name=%s}",feature->var_name);
  }
  
}


/*====================================================
  EVENT STUFF
  ====================================================*/
/*typedef enum {BE_EVENT_T, BE_EVENT_B} BE_event_type;

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
  } BE_event;*/
  
typedef struct BE_hook
{
  BE_event * event;
  ME_RLI_IR_expr * action;
  bool enabled;
}
BE_hook;

/*typedef struct BE_hook_array {
  BE_hook * hooks[64]; //MAX hook array size???
  int count=0;
}
BE_hook_array;*/

BE_event * BE_event_t_create(int delay, int repeat) {
  BE_event * event = (BE_event*)malloc(sizeof(BE_event));
  event->type = BE_EVENT_T;
  event->edata.t.delay = delay;
  event->edata.t.start = clock();
  event->repeat = repeat;
  return event;
}

BE_event * BE_event_b_create(int bp_id, char * filename, int line, int repeat) {
  BE_event * event = (BE_event*)malloc(sizeof(BE_event));
  event->type = BE_EVENT_B;
  event->edata.b.filename = (char*)malloc(sizeof(char) * strlen(filename)+1);
  memcpy(event->edata.b.filename, filename, sizeof(char) * strlen(filename)+1);
  
  event->edata.b.line = line;
  event->edata.b.bp_id = bp_id;
  event->repeat = repeat;
  return event;
}

void BE_event_print(BE_event * event) {
  
  if (event->type == BE_EVENT_T) {
    printf("{type=T,delay=%d}",event->edata.t.delay);
  } else if (event->type == BE_EVENT_B) {
    printf("{type=B,filename=%s,line=%d}",event->edata.b.filename,event->edata.b.line);
  }
}


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
    execute_command("interrupt");
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

void BE_start_session(struct BE_Context * bec)
{
  if (bec->driverfd!=-1)
  {
    printf("Session already started!\n");
    exit(-1);
  }
  bec->driverfd = ME_sock_server_connect();
}

BE_Context* BE_context_create(void)
{
  /*BE_Context* bec = (BE_Context*)malloc(sizeof(BE_Context));
  bec->attached = false;
  bec->stopped = false;
  bec->PID = NULL;
  bec->driverfd = -1;
  bec->CG_tracking = false;
  bec->CG_lasttime = 0;
  bec->CG = NULL;
  bec->FT = ME_FT_create("root");

  bec->et = BE_hook_table_create();
  
  return bec;*/
  the_context.attached = false;
  the_context.stopped = false;
  the_context.PID = NULL;
  the_context.driverfd = -1;
  the_context.CG_tracking = false;
  the_context.CG_lasttime = 0;
  the_context.CG = NULL;
  the_context.FT = ME_FT_create("root");

  //the_context.et = BE_hook_table_create();
  
  return &the_context;
  
}

void BE_context_print(struct BE_Context * bec)
{
  printf("Measurer Context {PID=%s,CG_tracking=%d,CG_lasttime=%ju,",bec->PID,bec->CG_tracking,(uintmax_t)bec->CG_lasttime);
  printf("\nCG=");
  ME_CG_print(bec->CG,bec->FT);
  printf(",\nFT=");
  ME_FT_print(bec->FT);
  printf("}\n");
}

bool startsWith(const char *pre, const char *str)
{
  size_t lenpre = strlen(pre),
    lenstr = strlen(str);
  return lenstr < lenpre ? false : strncmp(pre, str, lenpre) == 0;
}

void BE_get_request(struct BE_Context * bec)
{
  /*ME_FT * ft = ME_FT_create("root");
  ME_FT_add(ft, "main");
  ME_FT_add(ft, "foo");
  ME_FT_add(ft, "bar");
  ME_FT_add(ft, "bar");
  ME_FT_print(ft);
  printf("index of foo = %i\n", ME_FT_get_index(ft, "foo"));
  printf("value at 2 = %s\n", ME_FT_get(ft, 2));
  exit(-1);*/
  /*
  ME_CG * cg = ME_CG_create(ME_FT_add(bec->FT,"main"));
  ME_CG * cg2 = ME_CG_create(ME_FT_add(bec->FT,"foo"));
  ME_CG * cg3 = ME_CG_create(ME_FT_add(bec->FT,"bar"));
  ME_CG * cg4 = ME_CG_create(ME_FT_add(bec->FT,"bar"));
  ME_CG * cg5 = ME_CG_create(ME_FT_add(bec->FT,"printf"));
  ME_CG * cg6 = ME_CG_create(ME_FT_add(bec->FT,"printf"));
  ME_CG_add_child(cg, cg2);
  ME_CG_add_child(cg, cg3);
  ME_CG_add_child(cg2, cg4);
  ME_CG_add_child(cg3, cg5);
  ME_CG_add_child(cg4, cg6);
  ME_CG_print(cg,bec->FT);

  int count;
  int * cg_encoded;
  
  ME_CG_encode(cg, &count, &cg_encoded);

  ME_CG_print_encoded(count, cg_encoded);

  int ft_count;
  char * result;
  ME_FT_encode(bec->FT,&ft_count,&result);
  ME_FT_print_encoded(result);
  
  ME_FT * decoded_ft;
  
  ME_FT_decode(result, &decoded_ft);

  ME_FT_print(decoded_ft);*/

  /*
  int v=0;
  for (v=0; v<18; v++) {
    printf("%d,",cg_encoded[v]);00
  }
  printf("\n");
  
  ME_CG * decoded_cg = ME_CG_decode(cg_encoded);

  ME_CG_print(decoded_cg,bec->FT);*/

  //BE_context_print(bec);

  /*ME_CG * cg2_1 = ME_CG_create(ME_FT_add(bec->FT,"main"));
  ME_CG * cg2_2 = ME_CG_create(ME_FT_add(bec->FT,"bar"));
  ME_CG * cg2_3 = ME_CG_create(ME_FT_add(bec->FT,"printf"));
  ME_CG_add_child(cg2_1, cg2_2);
  ME_CG_add_child(cg2_2, cg2_3);
  ME_CG_print(cg2_1,bec->FT);

  ME_CG_merge_stack(cg, cg2_1);
  ME_CG_delete(cg2_1);
  
  ME_CG_print(cg,bec->FT);
  */
  //ME_CG_delete(cg2_1);
  //ME_CG_print(cg2_1,bec->FT);
  /*
  char * r1 = (char *)cg_encoded;
  int count_r1 = count * (sizeof(int)/sizeof(char));
  ME_sock_send_dynamic(bec->driverfd,count_r1,r1);
  ME_sock_send_dynamic(bec->driverfd,ft_count,result);
  exit(-1);*/

  /*if (bec->et==NULL) {
    bec->et = BE_hook_table_create();
    BE_hook_table_add(bec->et,BE_hook_t_create(10000000,1,1));
    BE_hook_table_add(bec->et,BE_hook_t_create(1000000,1,2));    
    }*/
  
  char request[1024];
  int n = ME_sock_recv(bec->driverfd, request);
  
  if (n > 0 && (*request))
    BE_rhandler_dispatch(bec, request);
    
}

void BE_update_callgraph(BE_Context * bec)
{
  
  printf("\nUpdate Call Graph!\n");
  //attach_command(bec->PID,1);
  //gdb_do_one_event ();
  //execute_command("bt",1);
  execute_command("interrupt");
  wait_for_inferior(); 
  normal_stop();
  struct ME_CG * stack;
  BE_get_call_stack_as_CG(NULL, 0, 0, 1, &stack, bec->FT);
  continue_command_JG();
  
  if (!bec->CG)
  {
    bec->CG = stack;
  }
  else
  {
    ME_CG_merge_stack(bec->CG,stack);
    ME_CG_delete(stack);
  }
  ME_CG_print(bec->CG,bec->FT);
  //execute_command("detach",1);

}


void BE_do_continuous(BE_Context * bec)
{
  
  if (!bec->attached) {
    return;
  }

  //Check inferior for stops
  char * filename = NULL;
  int line = -1;
  int breaked = fetch_inferior_event_JG(&filename, &line);
  if (breaked) {
    bec->stopped = true;
    printf("Stop caught at %s:%d !\n", filename, line);

    //BE_hook_table_handle(bec, bec->et, filename, line);
  }

  //Check events
  BE_hook_array_handle(bec, breaked, filename, line);

  if (bec->stopped) {
    continue_command_JG();
    bec->stopped = false;
  }
    
  clock_t t = clock();
  
  //Update Call Graph
  if (bec->CG_tracking && t >= bec->CG_lasttime + 10000000)
  {
    BE_update_callgraph(bec);
    bec->CG_lasttime = t;
  }
  
}

void BE_rhandler_dispatch(struct BE_Context * bec, const char * request)
{  
  char** args;
  int i;
  
  args = str_split(request, ' ');
  /*for (i = 0; *(args + i); i++)
  {
    printf("%i = \"%s\"\n",i,args[i]);
    }*/
  //exit(-1);

  printf("\nHandling %s request!\n",request);
  
  if (strcmp("quit",args[0])==0)
    ME_API_quit();
  else if (strcmp("print",args[0])==0)
    ME_API_print_context();
  else if (strcmp("set_target",args[0])==0)
    ME_API_set_target(args[1]);
  else if (strcmp("detach",args[0])==0)
    ME_API_detach();
  else if (strcmp("CG_begin",args[0])==0)
    BE_rhandler_CG_begin(bec);
  else if (strcmp("CG_end",args[0])==0)
    BE_rhandler_CG_end(bec);
  else if (strcmp("CG_get",args[0])==0)
    BE_rhandler_CG_get(bec);
  else if (strcmp("variable_get",args[0])==0)
    BE_rhandler_variable_get(bec, args[1]);
  else if (strcmp("variable_record_at",args[0])==0) {
    /*char** loc = str_split(args[1], ':');
    int line = atoi(loc[1]);

    BE_hook * bev = BE_event_b_create(get_breakpoint_count()+1,loc[0],line,0);
    BE_hook_set_feature(bev, BE_FEATURE_VARIABLE, args[2]);
    
    BE_hook_array_add(bev);
    
    //interrupt and insert breakpoint
    execute_command("interrupt");
    wait_for_inferior(); 
    normal_stop();
    break_command(args[1],0);

    //continue
    continue_command_JG();*/
  }
  else if (strcmp("callstack_get",args[0])==0)
    BE_rhandler_callstack_get(bec);
  else if (strcmp("callstack_get_at",args[0])==0)
    BE_rhandler_callstack_get_at(bec, args[1]);
  else if (strcmp("callstack_track",args[0])==0) {
    /*int delay = atoi(args[1]);

    BE_hook * bev = BE_event_t_create(delay,1);
    BE_hook_set_feature(bev, BE_FEATURE_CALLSTACK, NULL);
    BE_hook_array_add(bev);*/
  }
  else if (strcmp("callstack_record_delay",args[0])==0) {
    /*int delay = atoi(args[1]);

    BE_hook * bev = BE_event_t_create(delay,0);
    BE_hook_set_feature(bev, BE_FEATURE_CALLSTACK, NULL);
    BE_hook_array_add(bev);*/
  }
  else if (strcmp("callstack_record_at",args[0])==0) {
    /*char** loc = str_split(args[1], ':');
    int line = atoi(loc[1]);

    BE_hook * bev = BE_event_b_create(get_breakpoint_count()+1,loc[0],line,0);
    BE_hook_set_feature(bev, BE_FEATURE_CALLSTACK, NULL);
    BE_hook_array_add(bev);
    
    //interrupt and insert breakpoint
    execute_command("interrupt");
    wait_for_inferior(); 
    normal_stop();
    break_command(args[1],0);

    //continue
    continue_command_JG();
    */
  }  
  else if (strcmp("callstack_track_at",args[0])==0) {
    /*char** loc = str_split(args[1], ':');
    int line = atoi(loc[1]);

    BE_hook * bev = BE_event_b_create(get_breakpoint_count()+1,loc[0],line,1);
    BE_hook_set_feature(bev, BE_FEATURE_CALLSTACK, NULL);
    BE_hook_array_add(bev);
    
    //interrupt and insert breakpoint
    execute_command("interrupt");
    wait_for_inferior(); 
    normal_stop();
    break_command(args[1],0);

    //continue
    continue_command_JG();
    */
  }
  else if (strcmp("measurement_disable",args[0])==0) {
    int i = atoi(args[1]);
    BE_hook_disable(BE_hook_array_get(i));
    
  }
  else if (strcmp("measurement_enable",args[0])==0) {
    int i = atoi(args[1]);
    BE_hook_enable(BE_hook_array_get(i));
    
  }
  else if (strcmp("measurement_kill",args[0])==0) {
    int i = atoi(args[1]);
    BE_hook_kill(BE_hook_array_get(i));
    
  }
  else if (strcmp("measurement_get",args[0])==0) {
    int i = atoi(args[1]);    
    BE_hook * hook = BE_hook_array_get(i);
    if (!hook) {
      printf("No event %d!!!\n", i);
    }
    else {    
      //BE_hook_print(ev);
      //ME_measurement_print(ev->measurements);
      //ME_measurement_send_temp(bec->driverfd, ev->measurements);
      printf("Unimplemented!\n");
    }
  }
  
  else {
    ME_RLI_token * tokens = ME_RLI_tokenize(request);

    ME_RLI_IR_expr * expr = ME_RLI_IR_expr_parse(&tokens);
    //ME_RLI_IR_expr_print(expr);
    //printf("\n");
    
    ME_RLI_IR_value result = ME_RLI_IR_expr_eval(expr);

    printf("result = ");
    ME_RLI_IR_value_print(result);
    printf("\n");
    
    //TODO delete tokens, expr
    
    //printf("\nUnrecognized request! request=%s\n",request);
  }

  free_str_split(args);
}

void BE_rhandler_CG_begin(BE_Context * bec)
{  
  bec->CG_tracking = true;
}

void BE_rhandler_CG_end(BE_Context * bec)
{  
  bec->CG_tracking = false;
}

void BE_rhandler_CG_get(BE_Context * bec)
{
  if (!bec->CG||!bec->FT) return;
  
  //Send Type
  char response_type[1];
  response_type[0] = 1; 
  ME_sock_send_dynamic(bec->driverfd, 1, response_type);

  //Send CallGraph & FT
  char * encoded_cg;
  int n;  
  ME_CG_encode(bec->CG, &n, &encoded_cg);
  int encoded_cg_count = n * (sizeof(int)/sizeof(char));
  
  ME_sock_send_dynamic(bec->driverfd, encoded_cg_count, encoded_cg);

  char * encoded_ft;
  int encoded_ft_count;
  ME_FT_encode(bec->FT, &encoded_ft_count, &encoded_ft);
  ME_sock_send_dynamic(bec->driverfd, encoded_ft_count, encoded_ft);

  free(encoded_cg);
  free(encoded_ft);
}

void BE_rhandler_callstack_get(BE_Context * bec)
{  
  if (!bec->attached) {
    printf("Not attached to a process!\n");
    return;
  }

  //attach_command(bec->PID,1);
  //gdb_do_one_event ();
  printf("Executing a interrupt command\n");
  execute_command("interrupt");
  printf("Waiting for inferior\n");
  wait_for_inferior(); 
  printf("Normal stop\n");
  normal_stop();

  printf("getting callstack\n");
  struct ME_CG * stack;
  struct ME_FT * ft = ME_FT_create("root");
  printf("before\n");
  BE_get_call_stack_as_CG(NULL, 0, 0, 1, &stack, ft);
  printf("after\n");
  //execute_command("continue&");
  continue_command_JG();

  //Send Type
  char response_type[1];
  response_type[0] = 1; 
  ME_sock_send_dynamic(bec->driverfd, 1, response_type);
  
  //Send CallGraph & FT
  char * encoded_cg;
  int n;  
  ME_CG_encode(stack, &n, &encoded_cg);
  int encoded_cg_count = n * (sizeof(int)/sizeof(char));
  
  ME_sock_send_dynamic(bec->driverfd, encoded_cg_count, encoded_cg);

  char * encoded_ft;
  int encoded_ft_count;
  ME_FT_encode(ft, &encoded_ft_count, &encoded_ft);
  ME_sock_send_dynamic(bec->driverfd, encoded_ft_count, encoded_ft);

  printf("freeing encoded_cg\n");
  free(encoded_cg);
  printf("freeing encoded_ft\n");
  free(encoded_ft);

  printf("freeing ft\n");
  ME_FT_delete(ft);
  printf("freeing cg\n");
  ME_CG_delete(stack);
}

void BE_rhandler_callstack_get_at(BE_Context * bec, const char * location)
{  
  if (!bec->attached) {
    printf("Not attached to a process!\n");
    return;
  }

  //interrupt and insert breakpoint
  execute_command("interrupt");
  wait_for_inferior(); 
  normal_stop();
  break_command(location,0);

  //continue to breakpoint and grab callstack
  continue_command_JG();
  wait_for_inferior(); 
  normal_stop();
  
  struct ME_CG * stack;
  struct ME_FT * ft = ME_FT_create("root");
  printf("before\n");
  BE_get_call_stack_as_CG(NULL, 0, 0, 1, &stack, ft);
  printf("after\n");

  //remove breakpoint and continue
  delete_command(NULL, 0);
  continue_command_JG();

  //Send Type
  char response_type[1];
  response_type[0] = 1;
  ME_sock_send_dynamic(bec->driverfd, 1, response_type);
  
  //Send CallGraph & FT
  char * encoded_cg;
  int n;  
  ME_CG_encode(stack, &n, &encoded_cg);
  int encoded_cg_count = n * (sizeof(int)/sizeof(char));
  
  ME_sock_send_dynamic(bec->driverfd, encoded_cg_count, encoded_cg);

  char * encoded_ft;
  int encoded_ft_count;
  ME_FT_encode(ft, &encoded_ft_count, &encoded_ft);
  ME_sock_send_dynamic(bec->driverfd, encoded_ft_count, encoded_ft);

  printf("freeing encoded_cg\n");
  free(encoded_cg);
  printf("freeing encoded_ft\n");
  free(encoded_ft);

  printf("freeing ft\n");
  ME_FT_delete(ft);
  printf("freeing cg\n");
  ME_CG_delete(stack);
}

void BE_rhandler_variable_get(BE_Context * bec, char * variable)
{  
  if (!bec->attached) {
    printf("Not attached to a process!\n");
    return;
  }
  
  execute_command("interrupt");
  wait_for_inferior(); 
  normal_stop();

  char * value = BE_get_variable(variable, 0);
  printf("Value of %s = %s\n", variable, value);
  
  continue_command_JG();
}

void BE_rhandler_variable_record_at(BE_Context * bec, char * vairable) {

}

/*============================================================*/
void BE_hook_array_handle(BE_Context * bec, int breaked, char * filename, int line)
{  
  //check event_t
  int i = 0;
  for (i=0; i<bec->hook_array.count;i++) {
    BE_hook * curr = BE_hook_array_get(i);  
    clock_t t = clock();
    if (curr->enabled) {
      switch (curr->event->type) {
      case BE_EVENT_T:
	if (t >= curr->event->edata.t.start + curr->event->edata.t.delay) {

	  //stop if not stopped
	  if (!bec->stopped) {
	    //Abstract this away
	    execute_command("interrupt");
	    wait_for_inferior(); 
	    normal_stop();

	    BE_get_file_and_line(get_selected_frame(NULL), &filename, &line); //put these in the BEC

	    bec->stopped = true;
	    //continue_command_JG();
	  }    

	  
	  BE_hook_handle(bec, curr);
	  
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
  for (i=0; i<bec->hook_array.count;i++) {
    BE_hook * curr = BE_hook_array_get(i);  
    if (curr->enabled) {
      switch (curr->event->type) {
      case BE_EVENT_T:
	break;
      case BE_EVENT_B:
	if (breaked) {
	  if (strcmp(curr->event->edata.b.filename,filename)==0 && curr->event->edata.b.line==line)
	  {
	    printf("At the breakpoint for this event!\n");
	    BE_hook_handle(bec, curr);
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

void BE_hook_handle(BE_Context * bec, BE_hook * hook) {
  //printf("handling e%d! delay = %d, start = %d\n", i, ev->delay, ev->start);
  if (hook->action) {
    ME_RLI_IR_expr_eval(hook->action);
    return;
  }

  /*if (ev->feature.type == BE_FEATURE_CALLSTACK) {
    BE_ehandler_measure_callstack(bec, ev);
  } else if (ev->feature.type == BE_FEATURE_VARIABLE) {
    BE_ehandler_measure_variable(bec, ev);
    }*/
}

void BE_ehandler_print(BE_Context * bec)
{
  BE_context_print(bec);
}

void BE_ehandler_measure_callstack(BE_Context * bec, BE_hook * ev)
{  
  if (!bec->attached || !bec->stopped) {
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

void BE_ehandler_measure_variable(BE_Context * bec, BE_hook * ev)
{
  if (!bec->attached || !bec->stopped) {
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

void ME_API_set_target(char * target_PID)
{
  printf("ME_API_set_target(%s)\n",target_PID);
  
  if (the_context.attached) {
    ME_API_detach();
  }
  
  the_context.PID = malloc(sizeof(char) * strlen(target_PID));
  strcpy(the_context.PID, target_PID);

  //Attach to process
  attach_command(the_context.PID,1);
  gdb_do_one_event ();

  the_context.attached = true;
  
  continue_command_JG();
}

void ME_API_detach()
{
  if (the_context.attached) {
    //Detach
    execute_command("detach",1);

    //Update context
    the_context.attached = false;
    free(the_context.PID);
    the_context.PID == NULL;
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
  ME_measurement * ms;
  
  if (!the_context.attached) {
    printf("Not attached to a process!\n");
    return;
  }

  bool stopped_here = false;
  if (!the_context.stopped) {
    //Interrupt Inferior
    execute_command("interrupt");
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

ME_measurement * ME_API_measure(BE_feature * feature)
{
  ME_measurement * ms;
  
  if (!the_context.attached) {
    printf("Not attached to a process!\n");
    return;
  }

  bool stopped_here = false;
  if (!the_context.stopped) {
    //Interrupt Inferior
    execute_command("interrupt");
    wait_for_inferior(); 
    normal_stop();
    stopped_here = true;
  }
 
  //Measure Callstack
  if (feature->type == BE_FEATURE_CALLSTACK) {
    printf("getting callstack\n");
    struct ME_CG * stack;
    struct ME_FT * ft = ME_FT_create("root");
    printf("before\n");
    BE_get_call_stack_as_CG(NULL, 0, 0, 1, &stack, ft);
    printf("after\n");

    ms = ME_measurement_create(ME_MEASUREMENT_CALLSTACK);
    ms->data.cgft.cg = stack;
    ms->data.cgft.ft = ft;
  } else if (feature->type == BE_FEATURE_VARIABLE) {
    printf("getting var\n");
    char * value = BE_get_variable(feature->var_name, 0);
    
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
  BE_event * event = BE_event_b_create(get_breakpoint_count()+1,filename,line,repeat);

  //interrupt and insert breakpoint
  execute_command("interrupt");
  wait_for_inferior(); 
  normal_stop();
  char arg[64];
  sprintf(arg, "%s:%d", filename, line);
  break_command(arg,0);

  //continue
  continue_command_JG();
      
  return event;
}

int ME_API_hook(BE_event * event, ME_RLI_IR_expr * action) {
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

BE_feature * ME_API_callstack() {
  return BE_feature_create_callstack();
}

BE_feature * ME_API_var(char * var_name) {
  return BE_feature_create_variable(var_name);
}

int ME_API_add(int a, int b) {
  return a + b;
}   

int ME_API_subtract(int a, int b) {
  return a - b;
}
