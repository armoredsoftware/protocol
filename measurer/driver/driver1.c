#include "../gdb-7.9/gdb/ME_common.c"
#include "driver-interface-FE.c"

int main(int arc, char **argv)  {
  char * PID = argv[1];
  
  int sockfd = DI_init_measurer();

  /*int n;
  char * message;
  
  int n2;
  char * encoded_ft;
  
  BE_sock_recv_dynamic(sockfd, &n, &message);
  BE_sock_recv_dynamic(sockfd, &n2, &encoded_ft);
  
  BE_CG_print_encoded(n / (sizeof(int)/sizeof(char)), (int *)message);
  BE_func_table_print_encoded(encoded_ft);

  BE_func_table * decoded_ft;
  BE_func_table_decode(encoded_ft, &decoded_ft);
  BE_func_table_print(decoded_ft);
  
  BE_CG * decoded_cg;
  BE_CG_decode(message, &decoded_cg);

  printf("decoded\n");
  
  BE_CG_print(decoded_cg,decoded_ft);
  
  exit(-1);*/


  DI_interactive_mode(sockfd);
  
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
  
  sleep(5);
  
  //DI_send_request(sockfd, "quit\n");
  DI_interactive_mode(sockfd);
  
}
