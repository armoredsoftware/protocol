#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <sys/types.h>
#include <time.h> 

#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <libxenvchan.h>

//#define send send2
#include <exp1Common.h>
//#undef send

int main(int argc, char *argv[])
{

	//Vchan Prep
	struct libxenvchan *chan = 0;
	int selfId; // domainID of this node;
	int otherId;
	int client= 0;
	xentoollog_logger_stdiostream * xc_logger;
	xc_logger = createDebugLogger();
	selfId =getDomId();
	fprintf(stdout,"Client: Domain Id: %d\n", selfId);

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

	//Sockets Prep

	int listenfd = 0, connfd = 0;
	struct sockaddr_in serv_addr; 
	char recvBuff[1024];
	int n = 0;

	char sendBuff[1024];
	time_t ticks; 

	listenfd = socket(AF_INET, SOCK_STREAM, 0);
	memset(&serv_addr, '0', sizeof(serv_addr));
	memset(sendBuff, '0', sizeof(sendBuff)); 

	serv_addr.sin_family = AF_INET;
	serv_addr.sin_addr.s_addr = htonl(INADDR_ANY);
	serv_addr.sin_port = htons(5000); 

	bind(listenfd, (struct sockaddr*)&serv_addr, sizeof(serv_addr)); 

	printf("\n Waiting for a client...\n");
	listen(listenfd, 10); 
	printf("\n 1\n");

	while(1)
	{
	printf("\n 2\n");
		connfd = accept(listenfd, (struct sockaddr*)NULL, NULL); 
	printf("\n 3\n");

		ticks = time(NULL);

		while(1)
		{


			//GET REQUEST
			printf("Waiting for request from Attester\n");
			int size;
			char * msg;
			int i = 0;

			libxenvchan_wait(chan);
			msg = vchan_receive(chan,&size);
			printf("Received: ");
			for(i = 0; i< size; i++){
				printf("%c",msg[i]);
			}
			printf("\n");

			//ISSUE REQUEST
			printf("Sending request message to Measurer\nsent: ");

			memset(sendBuff, 0, sizeof(sendBuff));
			snprintf(sendBuff, sizeof(sendBuff), msg);
			if(fputs(sendBuff, stdout) == EOF)
			{
	    			printf("\n Error : Fputs error\n");
			} 
			write(connfd, sendBuff, sizeof(sendBuff)); 

			//GET RESPONSE
			printf("Waiting for response from Measurer\n");

			n = read(connfd, recvBuff, sizeof(recvBuff));
			recvBuff[n] = 0;
			if(fputs(recvBuff, stdout) == EOF)
			{
				printf("\n Error : Fputs error\n");
			} 

			if(n < 0)
			{
				printf("\n Read error \n");
			} 

			//SEND RESPONSE TO ATTESTER
			printf("Sending response to Attester\n");
			//char * tmp = (char *) malloc(256 * sizeof(char));
		     	//printf("Enter text to send:\n");
		     	//fgets(tmp, 256, stdin);
		     	vchan_send(chan,recvBuff, strlen(recvBuff)); 

		}

		close(connfd);
		sleep(1);
	}
}
