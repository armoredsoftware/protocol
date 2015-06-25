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
#include <jansson.h>

#include <readline/readline.h>
#define clock history_clock
#include <readline/history.h>

int
DI_init_measurer (void)
{
  //printf("Launching GDB...\n");
  //system("/projects/zephyr/deadzone/ArmoredSoftware/llvm/gdb/mod1/gdb-7.9/gdb/gdb");

  printf("Connect socket...\n");
  int sockfd = 0, n = 0;
  char recvBuff[1024];
  struct sockaddr_in serv_addr;

  memset(recvBuff, '0',sizeof(recvBuff));
  if((sockfd = socket(AF_INET, SOCK_STREAM, 0)) < 0)
    {
      printf("\n Error : Could not create socket \n");
      return 1;
    }

  memset(&serv_addr, '0', sizeof(serv_addr));

  serv_addr.sin_family = AF_INET;
  serv_addr.sin_port = htons(5000);
  
  if(inet_pton(AF_INET, "127.0.0.1", &serv_addr.sin_addr)<=0)
  {
    printf("\n inet_pton error occured\n");
    return 1;
  }

  if( connect(sockfd, (struct sockaddr *)&serv_addr, sizeof(serv_addr)) < 0)
  {
    printf("\n Error : Connect Failed \n");
    exit(-1);
    //return 1;
  }

  //fcntl(sockfd, F_SETFL, O_NONBLOCK);
  
  printf("Connected to measurer!\n");

  return sockfd;
}


void DI_send_request(int sockfd, char * request)
{
  //build JSON and send
  json_t *root, *result;
  json_error_t error;
  root = json_object();
  json_object_set_new(root, "jsonrpc", json_string("2.0"));
  json_object_set_new(root, "method", json_string("eval"));
  json_object_set_new(root, "params", json_string(request));   
  json_object_set_new(root, "id", json_string("1"));

  char * send = json_dumps(root, 0);
  
  ME_sock_send(sockfd, send);

  json_decref(root);


  //get response
  char response[1024];
  int n = ME_sock_recv(sockfd, response);

  printf("Recieved:%s\n",response);

  root = json_loads(response, 0, &error);
  result = json_object_get(root,"result"); 
  
  printf("Result = %s\n",json_string_value(result));
}


void DI_get_response(int sockfd)
{
  char response[1024];
  
  ME_sock_recv(sockfd, response);

  printf("response:%s\n", response);
  
}

void DI_get_measurer_response(int sockfd)
{
  int message_type_size;
  char * message_type=NULL;
      
  ME_sock_recv_dynamic(sockfd, &message_type_size, &message_type);
   
  if (message_type_size==0||!message_type) return;
  
  if (message_type[0]==1)
  {
    printf("CG incoming...\n");
    
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

    ME_FT_delete(decoded_ft);
    ME_CG_delete(decoded_cg);
  }
  else if (message_type[0]==2) {
    ME_measurement * ms = ME_measurement_recieve(sockfd);
    ME_measurement_print(ms);
    //DELETE measurement...
  }
  else
  {
    printf("Unrecognized message type!\n");
  }

  free(message_type);
}

void DI_interactive_mode(int sockfd)
{
  rl_bind_key('\t',rl_abort); //disable auto-complete
  
  char* line=0;
  size_t line_buf_len=0;
  ssize_t curr_line_len;

  //set_conio_terminal_mode();
    
  fflush(stdout);
  
  while(true) {
    struct timeval tv = { 0L, 0L };
    fd_set fds;
    FD_ZERO(&fds);
    //FD_SET(0, &fds);
    FD_SET(sockfd, &fds);
        
    line = readline("\n(me) ");
    //curr_line_len=getline(&line, &line_buf_len, stdin);

    DI_send_request(sockfd, line);

    //fflush(stdout);
    if (line[0]!=0)
      add_history(line);

    /*int readsocks = select(sockfd+1, &fds, NULL, NULL, &tv);
    if (readsocks < 0) {
      printf("Select read error!\n");
      exit(-1);
    }	
    if (FD_ISSET(sockfd,&fds)) {
      DI_get_measurer_response(sockfd); 
      }*/
      
  }
}
