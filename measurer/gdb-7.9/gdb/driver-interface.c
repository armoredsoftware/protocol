#include "defs.h"
#include "driver-interface.h"
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

#include <sys/socket.h>
#include <sys/types.h>
#include <netinet/in.h>
#include <netdb.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <arpa/inet.h> 

#include <stdbool.h>

#include <assert.h>

typedef struct BE_CG
{
  struct BE_CG * child;
  struct BE_CG * sibling;
  int symbol;  
}
BE_CG;


typedef struct BE_func_table
{
  struct BE_func_table * next;
  char * name;  
}
BE_func_table;


BE_CG * BE_CG_create(int symbol)//char * name)
{
  BE_CG* cg = (BE_CG*)malloc(sizeof(BE_CG));
  cg->child = NULL;
  cg->sibling = NULL;
  cg->symbol = symbol;
  return cg;
}

void BE_CG_add_child(struct BE_CG * parent, struct BE_CG * child)
{
  if (parent->child == NULL) {
    parent->child = child;
    return;
  }

  BE_CG * curr = parent->child;
  while (curr->sibling) {
    curr = curr->sibling;
  } 
  curr->sibling = child;
}

/*BE_CG * BE_CG_add_child(struct BE_CG * parent, int child_symbol)
{
  if (parent->child == NULL) {
    parent->child = child;
    return;
  }

  BE_CG * curr = parent->child;
  if (curr->symbol==child_symbol) return curr;
  while (curr->sibling) {
    curr = curr->sibling;
    if (curr->symbol==child_symbol) return curr;
  }
  BE_CG * child = BE_CG_create(child_symbol);  
  curr->sibling = child;
  return child;
  }*/

BE_CG * BE_CG_copy(struct BE_CG * cg)
{
  if (cg == NULL) return NULL;
  
  BE_CG * copy = BE_CG_create(cg->symbol);
  copy->sibling = BE_CG_copy(cg->sibling);
  copy->child = BE_CG_copy(cg->child);

  return copy;      
}

void BE_CG_delete(struct BE_CG * cg)
{
  if (cg->sibling)
  {
    BE_CG_delete(cg->sibling);
  }    
  if (cg->child)
  {
    BE_CG_delete(cg->child);
  }
  free(cg);
}

void BE_CG_merge_stack(struct BE_CG * cg, struct BE_CG * stack)
{
  if (!cg) {
    printf("No cg to merge to!!!\n");
    exit(-1);
  }
  
  if (!stack) return;
  
  BE_CG * curr = cg;
  while (curr->sibling)
  {
    if (curr->symbol == stack->symbol)
    {
      if (curr->child==NULL) {
	curr->child = BE_CG_copy(stack->child);
	return;
      }
      return BE_CG_merge_stack(curr->child,stack->child);
    }
    curr = curr->sibling;
  }  
  if (curr->symbol == stack->symbol)
  {
    if (curr->child==NULL) {
      curr->child = BE_CG_copy(stack->child);
      return;
    }
    return BE_CG_merge_stack(curr->child,stack->child);
  }
  curr->sibling = BE_CG_copy(stack);
}

void BE_CG_print_h(struct BE_CG * cg, struct BE_func_table * ft) {
  printf("(%s ", BE_func_table_get(ft,cg->symbol));

  if (cg->child) {
    BE_CG * curr = cg->child;
    BE_CG_print_h(curr, ft);
    while (curr->sibling) {
      curr = curr->sibling;
      BE_CG_print_h(curr, ft);
    }   
  }

  printf(")");
  
}

void BE_CG_print(struct BE_CG * cg, struct BE_func_table * ft) {
  if (cg==NULL) return;
  BE_CG_print_h(cg, ft);
  printf("\n");  
}

int BE_CG_count(struct BE_CG * cg) {
  if (cg == NULL) return 0;
  int i = 1;
  
  i += BE_CG_count(cg->sibling);
  i += BE_CG_count(cg->child);

  return i;
}

int BE_CG_encode_h(struct BE_CG * cg, int next, int * result)
{
  if (cg == NULL) return next;

  int i_this = next;
  next += 3;
  
  result[i_this] = cg->symbol;

  if (cg->child) {
    int i_child = next;
    next = BE_CG_encode_h(cg->child, i_child, result);
    result[i_this+1] = i_child;
  } else {
    result[i_this+1] = -1;
  }

  if (cg->sibling) {  
    int i_sibling = next;
    next = BE_CG_encode_h(cg->sibling, next, result);
    result[i_this+2] = i_sibling;
  } else {
    result[i_this+2] = -1;
  }
    
  return next; 
}

void BE_CG_encode(struct BE_CG * cg, int * count, int ** result) {
  int n = BE_CG_count(cg);
  printf("count = %d\n", n);

  (*result) = malloc(sizeof(int) * n * 3); 

  int n3 = BE_CG_encode_h(cg, 0, (*result));

  printf("n3 = %d\n",n3);
  
  if (n*3!=n3)
  {
    printf("Count and encode count are not equivalent!\n");
    //exit(-1);
  }

  //int i=0;
  //for (i=0; i<n*3; i+=3) printf("[%d]%d,%d,%d\n",i,(*result)[i],(*result)[i+1],(*result)[i+2]);

  (*count) = n3;
}

void BE_CG_print_encoded(int n, int * cg_encoded)
{
  int i=0;
  for (i=0; i<n; i+=3) printf("[%d]%d,%d,%d\n",i,cg_encoded[i],cg_encoded[i+1],cg_encoded[i+2]);
}

BE_CG * BE_CG_decode_h(int * a, int i)
{
  
  BE_CG * cg = BE_CG_create(a[i]);
  //printf("create sym %d from index %i\n", a[i],i);
  if (a[i+1]!=-1) {
    cg->child = BE_CG_decode_h(a, a[i+1]);
  }
  if (a[i+2]!=-1) {
    cg->sibling = BE_CG_decode_h(a, a[i+2]);
  }
  return cg;
}


BE_CG * BE_CG_decode(int * cg_encoded)
{
  
  int v=0;
  for (v=0; v<18; v++) {
    printf("%d,",cg_encoded[v]);
  }
  printf("\n");

  return BE_CG_decode_h(cg_encoded, 0);
}

BE_func_table* BE_func_table_create(char * name)
{
  BE_func_table* ft = (BE_func_table*)malloc(sizeof(BE_func_table));
  ft->next = NULL;
  ft->name = name;
  return ft;
}

int BE_func_table_add(BE_func_table * ft, char * name)
{
  BE_func_table * new_entry = BE_func_table_create(name);

  int i = 0;
  
  BE_func_table * curr = ft;
  while (curr->next) {
    i++;
    curr = curr->next;
    if (strcmp(curr->name,name)==0) return i;
  }
  i++;
  curr->next = new_entry;
  return i;
}

void BE_func_table_print(BE_func_table * ft)
{
  BE_func_table * curr = ft;
  int i = 0;
  while (curr->next) {
    i++;
    curr = curr->next;
    printf("%d:%s ", i, curr->name);
  }
  printf("\n");
  
}

int BE_func_table_get_index(BE_func_table * ft, char * name)
{
  int i = 0;
  BE_func_table * curr = ft;
  while (curr->next) {
    i++;
    curr = curr->next;
    if (strcmp(curr->name,name)==0) return i;
  }
  return -1;
}

char * BE_func_table_get(struct BE_func_table * ft, int i)
{
  BE_func_table * curr = ft;
  while (curr->next) {
    i--;
    curr = curr->next;
    if (i==0) return curr->name;
  }
  return NULL;

}

typedef struct BE_Context
{
  char * PID;
  int driverfd;
  bool CG_tracking;
  clock_t CG_lasttime;
  BE_CG * CG;
  BE_func_table * FT;
}
BE_Context;

int BE_sock_connect(void)
{
  int listenfd = 0, connfd = 0;
  struct sockaddr_in serv_addr;
  
  printf("Listening for client...\n");
   
  listenfd = socket(AF_INET, SOCK_STREAM, 0);
  memset(&serv_addr, '0', sizeof(serv_addr));

  serv_addr.sin_family = AF_INET;
  serv_addr.sin_addr.s_addr = htonl(INADDR_ANY);
  serv_addr.sin_port = htons(5000);

  bind(listenfd, (struct sockaddr*)&serv_addr, sizeof(serv_addr));

  listen(listenfd, 10); 
 
  connfd = accept(listenfd, (struct sockaddr*)NULL, NULL);

  printf("Connected to client!\n");
  //ticks = time(NULL);
  //snprintf(sendBuff, sizeof(sendBuff), "%.24s\r\n", ctime(&ticks));
  //write(connfd, sendBuff, strlen(sendBuff));

  //close(connfd);

  fcntl(connfd, F_SETFL, O_NONBLOCK);
  
  return connfd;
}

int BE_sock_recv(int sockfd, char * message)
{
  //char * recvBuff = (*result);
  char recvBuff[1024];
  int n;
  
  memset(recvBuff, 0, sizeof(recvBuff));
    
  n = read(sockfd, recvBuff, sizeof(recvBuff));

  if (n < 0)
  {
    //printf("\n Error: Read error!\n");
    return n;
  }
  
  recvBuff[strlen(recvBuff)-1] = 0;

  strcpy(message,recvBuff);
  
  return n;
}

void BE_sock_send(int sockfd, char * message)
{
  char sendBuff[1025];

  memset(sendBuff, 0 ,sizeof(sendBuff));

  snprintf(sendBuff, sizeof(sendBuff), message);
  printf("Sending:%s\n", sendBuff);

  write(sockfd, sendBuff, sizeof(sendBuff)-1);

}

int BE_sock_recv_dynamic(int sockfd, int * n, char ** message)
{
  //n = (int *)malloc(sizeof(int));
     
  int n1 = read(sockfd, n, sizeof(int));

  printf("n = %d", (*n));
  
  if (n < 0)
  {
    //printf("\n Error: Read error!\n");
    return n;
  }
  /*
  recvBuff[strlen(recvBuff)-1] = 0;

  strcpy(message,recvBuff);
  
  return n;*/
  return 0;
}


void BE_sock_send_dynamic(int sockfd, int n, char * message)
{
  /*  n = 1024;
  char * m = (char *)malloc(sizeof(int));
  memcpy(m, &n, sizeof(int));

  int * np = (int *)malloc(sizeof(int));
  memcpy(np, m, sizeof(int));
  
  printf("n=%d,&n=%d,np=%d,*,*np=%d\n", n, &n, np, (*np));
  */
  //memset((*m), 0, sizeof(m));

  //m[0] = 'a';
  //m[1] = 'b';
  //m[2] = '\n';
  //m[3] = 0;
  //char l[1025];
  //memset(l, 0, sizeof(l));
  //snprintf(l, sizeof(l), "ab\n");
  
  
  //write(sockfd, (char *)(&n), sizeof(int));
  int count = write(sockfd, &n, sizeof(int));
  //write(sockfd, m, sizeof(int));
  //write(sockfd, m, sizeof(int));

  printf("%d written!\n",count);

  count = write(sockfd, message, sizeof(char)*n);
  
  sleep(5);
  exit(-1);
  //write(sockfd, message, n);

}

void BE_start_session(struct BE_Context * bec)
{
  if (bec->driverfd!=-1)
  {
    printf("Session already started!\n");
    exit(-1);
  }
  bec->driverfd = BE_sock_connect();
}

BE_Context* BE_context_create(void)
{
  BE_Context* bec = (BE_Context*)malloc(sizeof(BE_Context));
  bec->PID = NULL;
  bec->driverfd = -1;
  bec->CG_tracking = false;
  bec->CG_lasttime = 0;
  bec->CG = NULL;
  bec->FT = BE_func_table_create("root");
  return bec;
}

void BE_context_print(struct BE_Context * bec)
{
  printf("Measurer Context {PID=%s,CG_tracking=%d,CG_lasttime=%d,",bec->PID,bec->CG_tracking,bec->CG_lasttime);
  printf("\nCG=");
  BE_CG_print(bec->CG,bec->FT);
  printf(",\nFT=");
  BE_func_table_print(bec->FT);
  printf("}\n");
}

bool startsWith(const char *pre, const char *str)
{
  size_t lenpre = strlen(pre),
    lenstr = strlen(str);
  return lenstr < lenpre ? false : strncmp(pre, str, lenpre) == 0;
}

char** str_split(char* a_str, const char a_delim)
{
  char** result    = 0;
  size_t count     = 0;
  char* tmp        = a_str;
  char* last_comma = 0;
  char delim[2];
  delim[0] = a_delim;
  delim[1] = 0;

  /* Count how many elements will be extracted. */
  while (*tmp)
    {
      if (a_delim == *tmp)
	{
	  count++;
	  last_comma = tmp;
	}
      tmp++;
    }

  /* Add space for trailing token. */
  count += last_comma < (a_str + strlen(a_str) - 1);

  /* Add space for terminating null string so caller
     knows where the list of returned strings ends. */
  count++;

  result = malloc(sizeof(char*) * count);

  if (result)
    {
      size_t idx  = 0;
      char* token = strtok(a_str, delim);

      while (token)
	{
	  assert(idx < count);
	  *(result + idx++) = strdup(token);
	  token = strtok(0, delim);
	}
      assert(idx == count - 1);
      *(result + idx) = 0;
    }

  return result;
}

void BE_get_request(struct BE_Context * bec)
{
  /*BE_func_table * ft = BE_func_table_create("root");
  BE_func_table_add(ft, "main");
  BE_func_table_add(ft, "foo");
  BE_func_table_add(ft, "bar");
  BE_func_table_add(ft, "bar");
  BE_func_table_print(ft);
  printf("index of foo = %i\n", BE_func_table_get_index(ft, "foo"));
  printf("value at 2 = %s\n", BE_func_table_get(ft, 2));
  exit(-1);*/

  BE_CG * cg = BE_CG_create(BE_func_table_add(bec->FT,"main"));
  BE_CG * cg2 = BE_CG_create(BE_func_table_add(bec->FT,"foo"));
  BE_CG * cg3 = BE_CG_create(BE_func_table_add(bec->FT,"bar"));
  BE_CG * cg4 = BE_CG_create(BE_func_table_add(bec->FT,"bar"));
  BE_CG * cg5 = BE_CG_create(BE_func_table_add(bec->FT,"printf"));
  BE_CG * cg6 = BE_CG_create(BE_func_table_add(bec->FT,"printf"));
  BE_CG_add_child(cg, cg2);
  BE_CG_add_child(cg, cg3);
  BE_CG_add_child(cg2, cg4);
  BE_CG_add_child(cg3, cg5);
  BE_CG_add_child(cg4, cg6);
  BE_CG_print(cg,bec->FT);

  int count;
  int * cg_encoded;
  
  BE_CG_encode(cg, &count, &cg_encoded);

  BE_CG_print_encoded(count, cg_encoded);
  
  /*
  int v=0;
  for (v=0; v<18; v++) {
    printf("%d,",cg_encoded[v]);
  }
  printf("\n");
  
  BE_CG * decoded_cg = BE_CG_decode(cg_encoded);

  BE_CG_print(decoded_cg,bec->FT);*/

  //BE_context_print(bec);

  /*BE_CG * cg2_1 = BE_CG_create(BE_func_table_add(bec->FT,"main"));
  BE_CG * cg2_2 = BE_CG_create(BE_func_table_add(bec->FT,"bar"));
  BE_CG * cg2_3 = BE_CG_create(BE_func_table_add(bec->FT,"printf"));
  BE_CG_add_child(cg2_1, cg2_2);
  BE_CG_add_child(cg2_2, cg2_3);
  BE_CG_print(cg2_1,bec->FT);

  BE_CG_merge_stack(cg, cg2_1);
  BE_CG_delete(cg2_1);
  
  BE_CG_print(cg,bec->FT);
  */
  //BE_CG_delete(cg2_1);
  //BE_CG_print(cg2_1,bec->FT);

  char * r1 = (char *)cg_encoded;
  int count_r1 = count * (sizeof(int)/sizeof(char));
  BE_sock_send_dynamic(bec->driverfd,count_r1,r1);
  exit(-1);
  
  
  char request[1024];
  int n = BE_sock_recv(bec->driverfd, request);
  
  if (n > 0 && (*request))
    BE_rhandler_dispatch(bec, request);
    
}

void BE_update_callgraph(BE_Context * bec)
{
  printf("\nUpdate Call Graph!\n");
  attach_command(bec->PID,1);
  gdb_do_one_event ();
  //execute_command("bt",1);
  BE_CG * stack;
  BE_get_call_stack_as_CG(NULL, 0, 0, 1, &stack, bec->FT);
  if (!bec->CG)
  {
    bec->CG = stack;
  }
  else
  {
    BE_CG_merge_stack(bec->CG,stack);
    BE_CG_delete(stack);
  }
  BE_CG_print(bec->CG,bec->FT);
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
  }
  exit(-1);*/

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
  else if (strcmp("CG_get",args[0])==0) {
    BE_rhandler_CG_get(bec);
  }
  else {
    printf("\nUnrecognized request! request=%s\n",request);
    exit(-1);
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
  int * cg_encoded;
  int n;
  
  BE_CG_encode(bec->CG, &n, &cg_encoded);

  int n_char = n * (sizeof(int)/sizeof(char));

  printf("n i -> c = %d -> %d\n", n, n_char);

  char * cg_encoded_char = (char *)cg_encoded;
  
  BE_sock_send(bec->driverfd, cg_encoded);
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
