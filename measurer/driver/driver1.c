#include "driver-interface-FE.c"
//#include "../gdb-7.9/gdb/driver-interface.c"

int main(int arc, char **argv)  {
  printf("hello\n");
  
  char * PID = argv[1];
  
  int sockfd = DI_init_measurer();

  int n;
  char * message;
  /*char message[1024];
  DI_sock_recv(sockfd, message);
  printf("read:%s\n",message);*/

  BE_sock_recv_dynamic(sockfd, &n, &message);
  exit(-1);
  
  DI_send_request(sockfd, "print\n");
  
  char target_req[32];
  snprintf(target_req, sizeof(target_req), "set_target %s\n", PID);
  DI_send_request(sockfd, target_req);

  DI_send_request(sockfd, "print\n");
  
  sleep(5);
  
  DI_send_request(sockfd, "CG_begin\n");
  
  sleep(20);
  
  DI_send_request(sockfd, "print\n");

  sleep(10);
  
  DI_send_request(sockfd, "CG_end\n");

  sleep(5);
  
  DI_send_request(sockfd, "CG_get\n");
  DI_get_response(sockfd);
  
  sleep(5);
  
  DI_send_request(sockfd, "quit\n");

  
}
