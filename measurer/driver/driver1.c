#include "../gdb-7.9/gdb/ME_common.c"
#include "driver-interface-FE.c"

int main(int arc, char **argv)  {
  char * PID = argv[1];
 
  int sockfd = DI_init_measurer();

  DI_interactive_mode(sockfd);
 
  
}
