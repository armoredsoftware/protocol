#include "ME_common.h"

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

#include <sys/types.h>
#include <sys/time.h>

#include <stdbool.h>

#include <assert.h>

#include <fcntl.h>


char** str_split(char* a_str_O, const char a_delim)
{  
  char* a_str = malloc((strlen(a_str_O)+1)*sizeof(char));
  memcpy(a_str, a_str_O, (strlen(a_str_O)+1)*sizeof(char));
  
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

  free(a_str);
  
  return result;
}

void free_str_split(char **tokens) {
  int i;
  for (i = 0; *(tokens + i); i++)
  {
    free(*(tokens + i));
  }
  free(tokens);
}

/*====================================================
SOCKET STUFF
======================================================*/

int ME_sock_server_connect(void)
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

  fcntl(connfd, F_SETFL, O_NONBLOCK);

  close(listenfd);
  
  return connfd;
}

int ME_sock_recv(int sockfd, char * message)
{
  char recvBuff[1024];
  int n;

  memset(recvBuff, 0, sizeof(recvBuff));

  n = read(sockfd, recvBuff, sizeof(recvBuff));

  if (n < 0)
    {
      //printf("\n Error: Read error!\n");
      return n;
    }

  //printf("Recieved %d bytes:\"%s\"\n",n,message);

  recvBuff[strlen(recvBuff)-1] = 0;

  strcpy(message,recvBuff);

  return n;
}

void ME_sock_send(int sockfd, char * message)
{
  char sendBuff[1025];

  memset(sendBuff, 0 ,sizeof(sendBuff));

  snprintf(sendBuff, sizeof(sendBuff), "%s", message);
  printf("Sending:%s\n", sendBuff);

  int count = write(sockfd, sendBuff, sizeof(sendBuff)-1);

  //printf("Sent %d bytes:\"%s\"\n",count,sendBuff);

}

void ME_sock_recv_dynamic(int sockfd, int * n, char ** message)
{
  int n1 = read(sockfd, n, sizeof(int));

  if (!(n1>0&&n))
  {
    (*n)=0;
    return;
  }
      
  (*message) = (char *)malloc(sizeof(char)*(*n));

  int n2 = read(sockfd, (*message), sizeof(char)*(*n));

  printf("Recieved %d bytes:\"%s\"\n",(*n),(*message));
  
}


void ME_sock_send_dynamic(int sockfd, int n, char * message)
{
  int count = write(sockfd, &n, sizeof(int));
  
  count = write(sockfd, message, sizeof(char)*n);

  printf("Sent %d bytes:\"%s\"\n",count,message);
}


/*====================================================
CALL GRAPH STUFF
======================================================*/

typedef struct ME_CG
{
  struct ME_CG * child;
  struct ME_CG * sibling;
  int symbol;
}
ME_CG;

ME_CG * ME_CG_create(int symbol)
{
  ME_CG* cg = (ME_CG*)malloc(sizeof(ME_CG));
  cg->child = NULL;
  cg->sibling = NULL;
  cg->symbol = symbol;
  return cg;
}

void ME_CG_add_child(struct ME_CG * parent, struct ME_CG * child)
{
  if (parent->child == NULL) {
    parent->child = child;
    return;
  }

  ME_CG * curr = parent->child;
  while (curr->sibling) {
    curr = curr->sibling;
  }
  curr->sibling = child;
}

/*ME_CG * ME_CG_add_child(struct ME_CG * parent, int child_symbol)
{
  if (parent->child == NULL) {
    parent->child = child;
    return;
  }

  ME_CG * curr = parent->child;
  if (curr->symbol==child_symbol) return curr;
  while (curr->sibling) {
    curr = curr->sibling;
    if (curr->symbol==child_symbol) return curr;
  }
  ME_CG * child = ME_CG_create(child_symbol);  
  curr->sibling = child;
  return child;
  }*/

ME_CG * ME_CG_copy(struct ME_CG * cg)
{
  if (cg == NULL) return NULL;

  ME_CG * copy = ME_CG_create(cg->symbol);
  copy->sibling = ME_CG_copy(cg->sibling);
  copy->child = ME_CG_copy(cg->child);

  return copy;
}

void ME_CG_delete(struct ME_CG * cg)
{
  if (cg==NULL) return;
  
  if (cg->sibling)
    {
      ME_CG_delete(cg->sibling);
    }
  if (cg->child)
    {
      ME_CG_delete(cg->child);
    }
  free(cg);
}

void ME_CG_merge_stack(struct ME_CG * cg, struct ME_CG * stack)
{
  if (!cg) {
    printf("No cg to merge to!!!\n");
    exit(-1);
  }

  if (!stack) return;

  ME_CG * curr = cg;
  while (curr->sibling) {
    if (curr->symbol == stack->symbol) {
      if (curr->child==NULL) {
	curr->child = ME_CG_copy(stack->child);
	return;
      }
      ME_CG_merge_stack(curr->child,stack->child);
      return;
    }
    curr = curr->sibling;
  }
  if (curr->symbol == stack->symbol) {
    if (curr->child==NULL) {
      curr->child = ME_CG_copy(stack->child);
      return;
    }
    ME_CG_merge_stack(curr->child,stack->child);
    return;
  }
  curr->sibling = ME_CG_copy(stack);
}

void ME_CG_print_s_h(struct ME_CG * cg) {
  printf("(%d ", cg->symbol);

  if (cg->child) {
    ME_CG * curr = cg->child;
    ME_CG_print_s_h(curr);
    while (curr->sibling) {
      curr = curr->sibling;
      ME_CG_print_s_h(curr);
    }
  }

  printf(")");

}

void ME_CG_print_s(struct ME_CG * cg) {
  if (cg==NULL) return;
  ME_CG_print_s_h(cg);
  printf("\n");
}

void ME_CG_print_h(struct ME_CG * cg, struct ME_FT * ft) {
  printf("(%s ", ME_FT_get(ft,cg->symbol));

  if (cg->child) {
    ME_CG * curr = cg->child;
    ME_CG_print_h(curr, ft);
    while (curr->sibling) {
      curr = curr->sibling;
      ME_CG_print_h(curr, ft);
    }
  }

  printf(")");

}

void ME_CG_print(struct ME_CG * cg, struct ME_FT * ft) {
  if (cg==NULL) return;
  ME_CG_print_h(cg, ft);
  printf("\n");
}

int ME_CG_count(struct ME_CG * cg) {
  if (cg == NULL) return 0;
  int i = 1;

  i += ME_CG_count(cg->sibling);
  i += ME_CG_count(cg->child);

  return i;
}

int ME_CG_encode_h(struct ME_CG * cg, int next, int * result)
{
  if (cg == NULL) return next;

  int i_this = next;
  next += 3;

  result[i_this] = cg->symbol;

  if (cg->child) {
    int i_child = next;
    next = ME_CG_encode_h(cg->child, i_child, result);
    result[i_this+1] = i_child;
  } else {
    result[i_this+1] = -1;
  }

  if (cg->sibling) {
    int i_sibling = next;
    next = ME_CG_encode_h(cg->sibling, next, result);
    result[i_this+2] = i_sibling;
  } else {
    result[i_this+2] = -1;
  }

  return next;
}

void ME_CG_encode(struct ME_CG * cg, int * count, char ** result) {
  int n = ME_CG_count(cg);
  printf("count = %d\n", n);

  (*result) = (char *)malloc(sizeof(int) * n * 3);

  int n3 = ME_CG_encode_h(cg, 0, (int *)(*result));

  if (n*3!=n3)
    {
      printf("Count and encode count are not equivalent!\n");
      //exit(-1);
    }

  //int i=0;
  //for (i=0; i<n*3; i+=3) printf("[%d]%d,%d,%d\n",i,(*result)[i],(*result)[i+1],(*result)[i+2]);

  (*count) = (n3) * (sizeof(int)/sizeof(char));
}

void ME_CG_print_encoded(int n, char * encoded_cg)
{
  int * cg_encoded_i = (int *)encoded_cg;
  int n_i = n * (sizeof(int)/sizeof(char));
  int i=0;
  for (i=0; i<n; i+=3) printf("[%d]%d,%d,%d\n",i,cg_encoded_i[i],cg_encoded_i[i+1],cg_encoded_i[i+2]);
}

ME_CG * ME_CG_decode_h(int * a, int i)
{
  ME_CG * cg = ME_CG_create(a[i]);
  //printf("create sym %d from index %i\n", a[i],i);
  if (a[i+1]!=-1) {
    cg->child = ME_CG_decode_h(a, a[i+1]);
  }
  if (a[i+2]!=-1) {
    cg->sibling = ME_CG_decode_h(a, a[i+2]);
  }
  return cg;
}


void ME_CG_decode(char * encoded_cg, struct ME_CG ** decoded_cg)
{
  (*decoded_cg) = ME_CG_decode_h((int *)encoded_cg, 0);
}

/*====================================================
FUNC TABLE STUFF
======================================================*/

typedef struct ME_FT
{
  struct ME_FT * next;
  char * name;
}
  ME_FT;

ME_FT* ME_FT_create(char * name)
{
  ME_FT* ft = (ME_FT*)malloc(sizeof(ME_FT));
  ft->next = NULL;
  
  ft->name = malloc((strlen(name)+1)*sizeof(char));
  memcpy(ft->name,name, (strlen(name)+1)*sizeof(char));
  
  return ft;
}

void ME_FT_delete(struct ME_FT * ft)
{
  if (ft==NULL) return;
  
  ME_FT_delete(ft->next);

  free(ft->name);
  free(ft);
}

int ME_FT_add(ME_FT * ft, char * name)
{  
  int i = 0;

  ME_FT * curr = ft;
  while (curr->next) {
    i++;
    curr = curr->next;
    if (strcmp(curr->name,name)==0) return i;
  }
  i++;

  ME_FT * new_entry = ME_FT_create(name);
  curr->next = new_entry;
  return i;
}

void ME_FT_print(ME_FT * ft)
{
  ME_FT * curr = ft;
  int i = 0;
  while (curr->next) {
    i++;
    curr = curr->next;
    printf("%d:%s ", i, curr->name);
  }
  printf("\n");

}

int ME_FT_get_index(ME_FT * ft, char * name)
{
  int i = 0;
  ME_FT * curr = ft;
  while (curr->next) {
    i++;
    curr = curr->next;
    if (strcmp(curr->name,name)==0) return i;
  }
  return -1;
}

char * ME_FT_get(struct ME_FT * ft, int i)
{
  ME_FT * curr = ft;
  while (curr->next) {
    i--;
    curr = curr->next;
    if (i==0) return curr->name;
  }
  return NULL;

}

void ME_FT_encode(struct ME_FT * ft, int * count, char ** result)
{
  (*count) = 0;
  
  ME_FT * curr = ft;
  while (curr->next) {
    curr = curr->next;
    //printf("len:%d\n",strlen(curr->name));
    (*count) += strlen(curr->name) + 1;
  }

  ME_FT_print(ft);
  
  printf("count = %d\n", (*count));

  (*result) = malloc(sizeof(char) * (*count));

  int next = 0;
  
  curr = ft;
  int curr_count;
  while (curr->next) {
    curr = curr->next;
    curr_count = strlen(curr->name);
    memcpy((*result)+next*sizeof(char),curr->name,sizeof(char)*curr_count);
    next+=curr_count;
    (*result)[next] = ' ';
    next++;
  }
  (*result)[next-1]=0;

  if (next!=(*count)) {
      printf("next != count !!!\n");
      exit(-1);
  }
  printf("next=%d,count=%d,result=%s\n",next,(*count),(*result));
  ME_FT_print_encoded((*result));
}

void ME_FT_decode(char * ft_encoded, ME_FT ** ft)
{
  (*ft) = ME_FT_create("root");
  
  char** names;
  int i;
  
  names = str_split(ft_encoded, ' ');

  for (i = 0; *(names + i); i++)
  {
    //printf("%i = \"%s\"\n",i,names[i]);
    ME_FT_add((*ft),names[i]);
  }

  free_str_split(names);
  
}
  
void ME_FT_print_encoded(char * ft_encoded)
{
  printf("encoded-ft{%s}\n", ft_encoded);
}

/*====================================================
MEASUREMENT STUFF
======================================================*/

typedef enum ME_measurement_type {
  ME_MEASUREMENT_CALLSTACK, ME_MEASUREMENT_STRING
} ME_measurement_type;

typedef struct ME_CG_AND_FT {
  struct ME_CG * cg;
  struct ME_FT * ft;
} ME_CG_AND_FT;

typedef union ME_measurement_data {
  struct ME_CG_AND_FT cgft;
  char * string_val;
  
} ME_measurement_data;
  
typedef struct ME_measurement
{
  //reference to command???
  //when
  int measured; //measurement taken?
  ME_measurement_type type;
  ME_measurement_data data;

  struct ME_measurement * next;
}
ME_measurement;

ME_measurement * ME_measurement_create(ME_measurement_type type)
{
  ME_measurement* ms = (ME_measurement*)malloc(sizeof(ME_measurement));
  if (type == ME_MEASUREMENT_CALLSTACK) {
    ms->data.cgft.cg = NULL;
    ms->data.cgft.ft = NULL;
  }
  else if (type == ME_MEASUREMENT_STRING) {
    ms->data.string_val = NULL;
  }
  ms->measured = 0;
  ms->type = type;
  ms->next = NULL;
  return ms;
}

void ME_measurement_delete(struct ME_measurement * ms)
{
  if (!ms) return;

  ME_measurement_delete(ms->next);

  if (ms->type == ME_MEASUREMENT_CALLSTACK) {
    ME_CG_delete((ME_CG *)ms->data.cgft.cg);
    ME_FT_delete((ME_FT *)ms->data.cgft.ft);
  }
  else if (ms->type == ME_MEASUREMENT_STRING) {
    free(ms->data.string_val);
  }
    
  free(ms);  
}

void ME_measurement_print(struct ME_measurement * ms)
{
  if (!ms) {
    printf("NULL\n");
    return;
  }
  
  printf("M{");
  printf("type=%d",ms->type);
  printf(", measured=%d", ms->measured);

  if (ms->type == ME_MEASUREMENT_CALLSTACK) {
    printf(", cg=");
    ME_CG_print(ms->data.cgft.cg, ms->data.cgft.ft);
    
    printf(", ft=");
    ME_FT_print(ms->data.cgft.ft);
  }
  else if (ms->type == ME_MEASUREMENT_STRING) {
    printf(", string_val=\"%s\"",ms->data.string_val);
  }
    
  printf(", next=");
  ME_measurement_print(ms->next);
  
  printf("}\n");
}

void ME_measurement_send(int sockfd, struct ME_measurement * ms) {
  if (!ms) {
    int i = -1;
    ME_sock_send_dynamic(sockfd, sizeof(int)/sizeof(char), &i);
    return;
  }     

  //send type
  ME_sock_send_dynamic(sockfd, sizeof(ms->type)/sizeof(char), &(ms->type));  

  if (ms->type == ME_MEASUREMENT_CALLSTACK) {
    //Send data 1 (callgraph)
    char * encoded_cg;
    int n;
    ME_CG_encode(ms->data.cgft.cg, &n, &encoded_cg);
    int encoded_cg_count = n * (sizeof(int)/sizeof(char));

    ME_sock_send_dynamic(sockfd, encoded_cg_count, encoded_cg);

    //send data 2 (ft)
    char * encoded_ft;
    int encoded_ft_count;
    ME_FT_encode(ms->data.cgft.ft, &encoded_ft_count, &encoded_ft);
    ME_sock_send_dynamic(sockfd, encoded_ft_count, encoded_ft);

    free(encoded_cg);
    free(encoded_ft);
  }
  else if (ms->type == ME_MEASUREMENT_STRING) {
    //send string
    ME_sock_send_dynamic(sockfd, strlen(ms->data.string_val)+1, ms->data.string_val);
  }
    
  ME_measurement_send(sockfd, ms->next);
  
}

void ME_measurement_send_temp(int sockfd, struct ME_measurement * ms) {
  char response_type[1];
  response_type[0] = 2;
  ME_sock_send_dynamic(sockfd, 1, response_type);
  
  ME_measurement_send(sockfd, ms);
}

ME_measurement * ME_measurement_recieve(int sockfd) {

  int message_type_size;
  char * message_type=NULL;   
  ME_sock_recv_dynamic(sockfd, &message_type_size, &message_type);

  ME_measurement_type type = *((ME_measurement_type*)(message_type));
  
  if (type==-1)
    return NULL;
    
  ME_measurement * ms = ME_measurement_create(type);

  if (ms->type == ME_MEASUREMENT_CALLSTACK) {
    int encoded_cg_count;
    char * encoded_cg;
    int encoded_ft_count;
    char * encoded_ft;

    ME_sock_recv_dynamic(sockfd, &encoded_cg_count, &encoded_cg);
    ME_sock_recv_dynamic(sockfd, &encoded_ft_count, &encoded_ft);

    ME_FT * decoded_ft;
    ME_FT_decode(encoded_ft, &decoded_ft);
    ME_CG * decoded_cg;
    ME_CG_decode(encoded_cg, &decoded_cg);
    printf("callgraph = ");
    ME_CG_print(decoded_cg,decoded_ft);
    printf("\n");

    free(encoded_cg);
    free(encoded_ft);

    ms->data.cgft.cg = decoded_cg;
    ms->data.cgft.ft = decoded_ft;
  }
  else if (ms->type == ME_MEASUREMENT_STRING) {
    int string_val_count;
    char * string_val;
    ME_sock_recv_dynamic(sockfd, &string_val_count, &string_val);
    ms->data.string_val = string_val;
    printf("String val recieved is \"%s\"\n",string_val);
  }
  
  ms->next = ME_measurement_recieve(sockfd);

}
