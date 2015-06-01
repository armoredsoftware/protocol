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

typedef struct BE_Context
{
  char * PID;
  int driverfd;
  bool CG_tracking;
  clock_t CG_lasttime;
  ME_CG * CG;
  ME_FT * FT;
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
  bec->PID = NULL;
  bec->driverfd = -1;
  bec->CG_tracking = false;
  bec->CG_lasttime = 0;
  bec->CG = NULL;
  bec->FT = ME_FT_create("root");
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
  
  char request[1024];
  int n = ME_sock_recv(bec->driverfd, request);
  
  if (n > 0 && (*request))
    BE_rhandler_dispatch(bec, request);
    
}

void BE_update_callgraph(BE_Context * bec)
{
  printf("\nUpdate Call Graph!\n");
  attach_command(bec->PID,1);
  gdb_do_one_event ();
  //execute_command("bt",1);
  struct ME_CG * stack;
  BE_get_call_stack_as_CG(NULL, 0, 0, 1, &stack, bec->FT);
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
  execute_command("detach",1);
 
}


void BE_do_continuous(BE_Context * bec)
{
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
  else {
    printf("\nUnrecognized request! request=%s\n",request);
  }
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
  char * encoded_cg;
  int n;  
  ME_CG_encode(bec->CG, &n, &encoded_cg);
  int encoded_cg_count = n * (sizeof(int)/sizeof(char));
  
  ME_sock_send_dynamic(bec->driverfd, encoded_cg_count, encoded_cg);

  char * encoded_ft;
  int encoded_ft_count;
  ME_FT_encode(bec->FT, &encoded_ft_count, &encoded_ft);
  ME_sock_send_dynamic(bec->driverfd, encoded_ft_count, encoded_ft);
}

void BE_rhandler_callstack_get(BE_Context * bec)
{

  printf("\nUpdate Call Graph!\n");
  attach_command(bec->PID,1);
  gdb_do_one_event ();
  //execute_command("bt",1);
  struct ME_CG * stack;
  struct ME_FT * ft = ME_FT_create("root");
  BE_get_call_stack_as_CG(NULL, 0, 0, 1, &stack, ft);
  execute_command("detach",1);
   
  char * encoded_cg;
  int n;  
  ME_CG_encode(stack, &n, &encoded_cg);
  int encoded_cg_count = n * (sizeof(int)/sizeof(char));
  
  ME_sock_send_dynamic(bec->driverfd, encoded_cg_count, encoded_cg);

  char * encoded_ft;
  int encoded_ft_count;
  ME_FT_encode(ft, &encoded_ft_count, &encoded_ft);
  ME_sock_send_dynamic(bec->driverfd, encoded_ft_count, encoded_ft);

}
 
void BE_rhandler_print(BE_Context * bec)
{
  BE_context_print(bec);
}

void BE_rhandler_set_target(BE_Context * bec, char * target_PID)
{
  free(bec->PID);
  
  bec->PID = malloc(sizeof(char) * strlen(target_PID));
  strcpy(bec->PID, target_PID);
}

void BE_rhandler_quit(BE_Context * bec)
{
  exit(-1);
}
