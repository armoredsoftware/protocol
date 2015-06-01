#include "defs.h"
#include "driver-interface.h"
#include "ME_common.c"
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
  EVENT STUFF
======================================================*/
typedef enum {BE_EVENT_T, BE_EVENT_B} BE_event_type;

typedef struct BE_event_t {
  int delay; //int time when
  clock_t start; //int time start
}
BE_event_t;

typedef struct BE_event_b {
  char * filename;
  int line;
}
BE_event_b;

typedef union BE_event_union {
  struct BE_event_t t;
  struct BE_event_b b;
}
BE_event_union;

typedef struct BE_event
{
  BE_event_type type;
  
  union BE_event_union data;
  
  int active;
  int repeat;
  
  //ACTION REFERENCE
  int action;

  //MEASUREMENTS
  struct ME_measurement * measurements;
  
  //FOR EVENT TABLE
  struct BE_event * next;

}
BE_event;

BE_event * BE_event_create_timed(int delay, int repeat, int action) {
  BE_event * ev = (BE_event*)malloc(sizeof(BE_event));
  ev->type = BE_EVENT_T;

  ev->data.t.delay = delay;
  ev->data.t.start = clock();
  //ev->delay = delay;
  //ev->start = clock();
  
  ev->active = 1;
  ev->repeat = repeat;

  ev->action = action;

  ev->measurements = NULL;
  
  ev->next = NULL;
  return ev;
}

BE_event * BE_event_b_create(char * filename, int line, int repeat, int action) {
  BE_event * ev = (BE_event*)malloc(sizeof(BE_event));
  ev->type = BE_EVENT_B;

  ev->data.b.filename = (char*)malloc(sizeof(char) * strlen(filename)+1);
  memcpy(ev->data.b.filename, filename, sizeof(char) * strlen(filename)+1);
  
  ev->data.b.line = line;
  
  ev->active = 1;
  ev->repeat = repeat;

  ev->action = action;

  ev->measurements = NULL;
  
  ev->next = NULL;
  return ev;
}

void BE_event_add_measurement(struct BE_event * ev, struct ME_measurement * ms) {
  ms->next = ev->measurements;
  ev->measurements = ms;
}

void BE_event_print(BE_event * ev) {
  if (!ev) {
    printf("NULL\n");
    return;
  }

  //INCOMPLETE
  
  printf("Event{type=%d,delay=%d,start=%d,active=%d,repeat=%d,action=%d,measurements=...,next=...\n");

}

BE_event * BE_event_table_create()
{
  //create null event
  return BE_event_create_timed(0,0,0);
}

void BE_event_table_add(BE_event * et, BE_event * ev) {
  BE_event * curr = et;
  while (curr->next) {
    curr = curr->next;
  }
  curr->next = ev;
}

BE_event * BE_event_table_get(BE_event * et, int i) {
  if (i<=0) return NULL;
  BE_event * curr = et;
  while (i && curr) {
    curr = curr->next;
    i--;
  }
  return curr;
}

/*====================================================
  CONTEXT STUFF
======================================================*/

typedef struct BE_Context
{
  bool attached;
  bool stopped;
  char * PID;
  int driverfd;
  bool CG_tracking;
  clock_t CG_lasttime;
  ME_CG * CG;
  ME_FT * FT;

  BE_event * et;
}
BE_Context;


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
  BE_Context* bec = (BE_Context*)malloc(sizeof(BE_Context));
  bec->attached = false;
  bec->stopped = false;
  bec->PID = NULL;
  bec->driverfd = -1;
  bec->CG_tracking = false;
  bec->CG_lasttime = 0;
  bec->CG = NULL;
  bec->FT = ME_FT_create("root");

  bec->et = BE_event_table_create();
  
  return bec;
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
    bec->et = BE_event_table_create();
    BE_event_table_add(bec->et,BE_event_create_timed(10000000,1,1));
    BE_event_table_add(bec->et,BE_event_create_timed(1000000,1,2));    
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

    //BE_event_table_handle(bec, bec->et, filename, line);
  }

  //Check events
  BE_event_table_handle(bec, bec->et, breaked, filename, line);

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
    BE_rhandler_quit(bec);
  else if (strcmp("print",args[0])==0)
    BE_rhandler_print(bec);
  else if (strcmp("set_target",args[0])==0)
    BE_rhandler_set_target(bec, args[1]);
  else if (strcmp("CG_begin",args[0])==0)
    BE_rhandler_CG_begin(bec);
  else if (strcmp("CG_end",args[0])==0)
    BE_rhandler_CG_end(bec);
  else if (strcmp("CG_get",args[0])==0)
    BE_rhandler_CG_get(bec);
  else if (strcmp("callstack_get",args[0])==0)
    BE_rhandler_callstack_get(bec);
  else if (strcmp("callstack_get_at",args[0])==0)
    BE_rhandler_callstack_get_at(bec, args[1]);
  else if (strcmp("callstack_track",args[0])==0) {
    BE_event_table_add(bec->et,BE_event_create_timed(10000000,1,2));
  }
  else if (strcmp("callstack_record_delay",args[0])==0) {
    BE_event_table_add(bec->et,BE_event_create_timed(10000000,0,2));
  }
  else if (strcmp("callstack_record_at",args[0])==0) {
    char** loc = str_split(args[1], ':');
    int line = atoi(loc[1]);
    
    BE_event_table_add(bec->et,BE_event_b_create(loc[0],line,0,2));
    
    //interrupt and insert breakpoint
    execute_command("interrupt");
    wait_for_inferior(); 
    normal_stop();
    break_command(args[1],0);

    //continue
    continue_command_JG();
    
  }  
  else if (strcmp("callstack_track_at",args[0])==0) {
    char** loc = str_split(args[1], ':');
    int line = atoi(loc[1]);
    
    BE_event_table_add(bec->et,BE_event_b_create(loc[0],line,1,2));
    
    //interrupt and insert breakpoint
    execute_command("interrupt");
    wait_for_inferior(); 
    normal_stop();
    break_command(args[1],0);

    //continue
    continue_command_JG();
    
  }
  else if (strcmp("measurement_get",args[0])==0) {
    int i = atoi(args[1]);    
    BE_event * ev = BE_event_table_get(bec->et,i);
    if (!ev) {
      printf("No event %d!!!\n", i);
    }
    else {    
      //BE_event_print(ev);
      ME_measurement_print(ev->measurements);
      ME_measurement_send_temp(bec->driverfd, ev->measurements);
      printf("Sent measurements!\n");
    }
  }
  else {
    printf("\nUnrecognized request! request=%s\n",request);
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
  execute_command("interrupt");
  wait_for_inferior(); 
  normal_stop();
  
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

void BE_rhandler_print(BE_Context * bec)
{
  BE_context_print(bec);
}

void BE_rhandler_set_target(BE_Context * bec, char * target_PID)
{
  if (bec->attached) {
    execute_command("detach",1);
    bec->attached = false;
    free(bec->PID);
  }
  
  bec->PID = malloc(sizeof(char) * strlen(target_PID));
  strcpy(bec->PID, target_PID);

  attach_command(bec->PID,1);
  gdb_do_one_event ();

  bec->attached = true;
  
  continue_command_JG();
}

void BE_rhandler_quit(BE_Context * bec)
{
  if (bec->attached) {  
    execute_command("detach",1);
    bec->attached = false;
    free(bec->PID);
  }
    
  exit(-1);
}

/*============================================================*/
void BE_event_table_handle(BE_Context * bec, BE_event * et, int breaked, char * filename, int line)
{
  BE_event * curr;
  //check event_t
  for (curr = et->next; curr; curr=curr->next) {  
    clock_t t = clock();
    if (curr->active) {
      switch (curr->type) {
      case BE_EVENT_T:
	if (t >= curr->data.t.start + curr->data.t.delay) {

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

	  
	  BE_event_handle(bec, curr);
	  
	  if (curr->repeat) {
	    curr->data.t.start = t;
	  } else {
	    curr->active = 0;
	  }
	  
	}
	break;
      case BE_EVENT_B:
	break;
      }
    }
  }

  //check event_b
  for (curr = et->next; curr; curr=curr->next) {  
    if (curr->active) {
      switch (curr->type) {
      case BE_EVENT_T:
	break;
      case BE_EVENT_B:
	if (breaked) {
	  if (strcmp(curr->data.b.filename,filename)==0 && curr->data.b.line==line)
	  {
	    printf("At the breakpoint for this event!\n");
	    BE_event_handle(bec, curr);
	    printf("Event handled\n");
	    if (!curr->repeat) {
	      //Remove breakpoint...
	      curr->active = 0;
	    }
	    
	  }
	}
      }
      
    }
  }  
}

void BE_event_handle(BE_Context * bec, BE_event * ev) {
  //printf("handling e%d! delay = %d, start = %d\n", i, ev->delay, ev->start);
  if (ev->action == 1) {
    BE_context_print(bec);
  }
  else if (ev->action == 2) {
    BE_ehandler_measure_callstack(bec, ev);
  }
}

void BE_ehandler_print(BE_Context * bec)
{
  BE_context_print(bec);
}

void BE_ehandler_measure_callstack(BE_Context * bec, BE_event * ev)
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

  ME_measurement * ms = ME_measurement_create(0);
  ms->data = stack;
  ms->data2 = ft;

  BE_event_add_measurement(ev, ms);
  
}
