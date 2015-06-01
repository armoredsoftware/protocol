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

  printf("Connected to measurer!\n");

  return sockfd;
}


void DI_send_request(int sockfd, char * request)
{
  ME_sock_send(sockfd, request);
}


void DI_get_response(int sockfd)
{
  char response[1024];
  
  ME_sock_recv(sockfd, response);

  printf("response:%s\n", response);
  
}

void DI_interactive_mode(int sockfd)
{
  char* line=0;
  size_t line_buf_len=0;
  ssize_t curr_line_len;
  
  //while ((curr_line_len=getline(&line, &line_buf_len, stdin))>0)
  while (true)
  {
    printf("Enter command to send to measurer:");

    curr_line_len=getline(&line, &line_buf_len, stdin);

    DI_send_request(sockfd, line);
    if (strcmp(line,"CG_get\n")==0
	|| strcmp(line,"callstack_get\n")==0 )
    {
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
    }
    else if (strcmp(line,"quit\n")==0)
    {
      close(sockfd);
      exit(0);
    }
    
  }
}
