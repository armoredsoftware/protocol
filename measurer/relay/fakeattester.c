#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <libxenvchan.h>
#include <exp1Common.h>

//####################################################################

int main(int argc, char **argv)
{
  struct libxenvchan *chan = 0;
  int selfId; // domainID of this node;
  int otherId;
  int client= 0;
  selfId =getDomId();
  fprintf(stdout,"Client: Domain Id: %d\n", selfId);
  xentoollog_logger_stdiostream * xc_logger;
  xc_logger = createDebugLogger();

  if(argc != 3){
   fprintf(stderr,"Error: Usage ./Client [server-0, client-1] [other domId]\n");
   exit(1);
  }  

  sscanf(argv[1],"%d",&client);
  sscanf(argv[2],"%d",&otherId);

  if (!client){
    chan = vchan_server_init((xentoollog_logger *) xc_logger, otherId);
  }else{
    chan = vchan_client_init((xentoollog_logger *) xc_logger, otherId);
  }
   

  while (1){

    //SEND REQUEST TO ATTESTER
    //char * tmp = (char *) malloc(1024 * sizeof(char));
     char tmp[1024];
	memset(tmp, 0, sizeof(tmp));
     printf("Enter request to send:\n");
     fgets(tmp, 1024, stdin);
     send(chan,tmp, sizeof(tmp));

     int size;
  char * msg;
  int i = 0;

    //SEND REQUEST TO ATTESTER
    libxenvchan_wait(chan);
    msg = vchan_receive(chan,&size);
    printf("Received: ");
    for(i = 0; i< size; i++){
      printf("%c",msg[i]);
    }
     printf("\n");

 }
    
  
 

  libxenvchan_close(chan);


  return 0;
}
