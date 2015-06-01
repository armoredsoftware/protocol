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
    return 1;
  }

  printf("Connected to server!\n");

  /*while ( (n = read(sockfd, recvBuff, sizeof(recvBuff)-1)) > 0)
    {
      recvBuff[n] = 0;
      if(fputs(recvBuff, stdout) == EOF)
	{
	  printf("\n Error : Fputs error\n");
	}
    }

  if(n < 0)
  {
     printf("\n Read error \n");
     }*/

  return sockfd;
}


void DI_send_request(int sockfd, char * request)
{
  printf("Sending request to %d...\n", sockfd);
  
  char sendBuff[1025];

  memset(sendBuff, 0 ,sizeof(sendBuff));
  
  snprintf(sendBuff, sizeof(sendBuff), request);
  printf("Sending:%s\n", sendBuff);

  write(sockfd, sendBuff, sizeof(sendBuff)-1); 

  printf("Request sent!\n");
}


void DI_get_response(int sockfd)
{
  char response[1024];
  
  DI_sock_recv(sockfd, response);

  printf("response:%s\n", response);
  
}


int DI_sock_recv(int sockfd, char * message)
{
  //char * recvBuff = (*result);
  char recvBuff[1024];
  int n;

  memset(recvBuff, 0, sizeof(recvBuff));

  n = read(sockfd, recvBuff, sizeof(recvBuff));

  printf("recieved %d\n", n);
  
  if (n < 0)
    {
      printf("\n Error: Read error!\n");
      return n;
    }

  recvBuff[strlen(recvBuff)-1] = 0;

  strcpy(message,recvBuff);

  return n;
}

int BE_sock_recv_dynamic(int sockfd, int * n, char ** message)
{
  int n1 = read(sockfd, n, sizeof(int));

  printf("Expecting %d bytes!",(*n));

  (*message) = (char *)malloc(sizeof(char)*(*n));

  int n2 = read(sockfd, (*message), sizeof(char)*(*n));

  int * as_int = (int *)(*message);
  int as_int_n = (*n) / (sizeof(int) / sizeof(char));
  printf("n -> n = %d -> %d\n", (*n), as_int_n);
  int i;
  for (i=0; i<as_int_n; i+=3)
  {
    printf("[%d]%d,%d,%d\n",i,as_int[i],as_int[i+1],as_int[i+2]);
  }
  
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

