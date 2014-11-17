#include "precompiled.hpp"
#include "runtime/task.hpp"
#include "runtime/vmThread.hpp"
#include "runtime/timer.hpp"
#include "runtime/java.hpp"
#include "runtime/vm_operations.hpp"
#include "oops/methodOop.hpp"

#include "runtime/TalkThread.hpp"

//sockets
#include <sys/types.h>
#include <netinet/in.h>
#include <netdb.h>
#include <stdio.h>
#include <unistd.h>
#include <arpa/inet.h> 
#include <string>


TalkThread* TalkThread::_talk_thread   = NULL;
volatile bool  TalkThread::_should_terminate = false;


TalkThread::TalkThread() : Thread() 
{
  assert(talk_thread() == NULL, "we can only allocate one TalkThread");
  if (os::create_thread(this, os::talk_thread)) {
    _talk_thread = this;

    //CHECK: we probably want this here, right?
    // Set the talk thread to the highest OS priority which should not be
    // used, unless a Java thread with priority java.lang.Thread.MAX_PRIORITY
    // is created. The only normal thread using this priority is the reference
    // handler thread, which runs for very short intervals only.
    // If the VMThread's priority is not lower than the TalkThread profiling
    // will be inaccurate.
    //EDIT changed this to NormPriority rather than max
    os::set_priority(this, MaxPriority);
    if (!DisableStartThread) {
      os::start_thread(this);
    }
  }
}


std::string TalkThread::ExecuteOperation(int local)
{
	//Prepare VM Operation
	VM_MonitorAction op;
	op.signal = 1001; //code for variable value

	op.method = "main";//new char[method.length()];
	//strcpy (op.method,method.c_str());
	op.local = local;

	VMThread::execute(&op);	
	std::string result = op.result;

	printf("\n Result of VM operation was:%s \n", result.c_str());

	return result;
}

void TalkThread::run() {
	//Leave this blank for now
	printf("ENTRY: TalkThread::run()\n");

	int sockfd = 0, n = 0;
	char recvBuff[1024];
    	char sendBuff[1024];
	struct sockaddr_in serv_addr; 

	memset(recvBuff, '0',sizeof(recvBuff));
	if((sockfd = socket(AF_INET, SOCK_STREAM, 0)) < 0)
	{
		printf("\n Error : Could not create socket \n");		
	} 

		memset(&serv_addr, '0', sizeof(serv_addr)); 

		serv_addr.sin_family = AF_INET;
		serv_addr.sin_port = htons(5000); 

	if(inet_pton(AF_INET, "127.0.0.1", &serv_addr.sin_addr)<=0)
	{
		printf("\n inet_pton error occured\n");
	} 

	if( connect(sockfd, (struct sockaddr *)&serv_addr, sizeof(serv_addr)) < 0)
	{
		printf("\n Error : Connect Failed \n");
	} 


	while(1){

		//GET REQUEST
	 	printf("\n TalkThread : Awaiting request\n");
		n=0;

		n = read(sockfd, recvBuff, sizeof(recvBuff));
		recvBuff[n] = 0;
		if(fputs(recvBuff, stdout) == EOF)
		{
		    printf("\n Error : Fputs error\n");
		} 

		if(n < 0)
		{
			printf("\n Read error \n");
			return;
		} 

		//EXECUTE OPERATION
		if (strncmp(recvBuff,"{\"WEvidenceDescriptor\":\"D0\"}", 28)==0)
		{
			std::string result = ExecuteOperation(1);
			snprintf(sendBuff, sizeof(sendBuff), "{\"WEvidencePiece\":{\"M0\":\"%s\"}}\n", result.c_str());
		}
		else if (strncmp(recvBuff,"{\"WEvidenceDescriptor\":\"D1\"}", 28)==0)
		{			
			std::string result = ExecuteOperation(2);
			snprintf(sendBuff, sizeof(sendBuff), "{\"WEvidencePiece\":{\"M1\":\"%s\"}}\n", result.c_str());
		}
		else if (strncmp(recvBuff,"{\"WEvidenceDescriptor\":\"D2\"}", 28)==0)
		{			
			std::string result = ExecuteOperation(3);
			snprintf(sendBuff, sizeof(sendBuff), "{\"WEvidencePiece\":{\"M2\":\"%s\"}}\n", result.c_str());
		}
		else if (strncmp(recvBuff,"{\"WEvidenceDescriptor\":\"DONE\"}", 28)==0)
		{			
			snprintf(sendBuff, sizeof(sendBuff), "{\"WEvidencePiece\":\"OK\"}\n");
		}			
		else
		{
			snprintf(sendBuff, sizeof(sendBuff), "Invalid Request!\n");
		}

		/*if (strncmp(recvBuff,"PID", 3)==0)
		{
			snprintf(sendBuff, sizeof(sendBuff), "PID=%d\n", getpid());
		}
		else if (strncmp(recvBuff,"NTIME", 5)==0)
		{
			time_t ticks = time(NULL);
       			snprintf(sendBuff, sizeof(sendBuff), "%.24s\r\n", ctime(&ticks));
		}
		else
		{
			snprintf(sendBuff, sizeof(sendBuff), "Invalid Request!\n");
		}*/

		//SEND RESULT

	 	printf("\n TalkThread : Sending result\n");

        	write(sockfd, sendBuff, strlen(sendBuff)); 
		

	}

	while(1){
		printf("tick!\n");
		sleep(5);	
	}
}

void TalkThread::start() {
  printf("ENTRY: TalkThread::start()\n");
  if (talk_thread() == NULL) {
    _should_terminate = false;
    // Create the single instance of WatcherThread
    new TalkThread();
  }
}

void TalkThread::stop() {
  printf("ENTRY: TalkThread::stop()\n");

  // it is ok to take late safepoints here, if needed
  MutexLocker mu(Terminator_lock);
  _should_terminate = true;
  OrderAccess::fence();  // ensure WatcherThread sees update in main loop


  Thread* talk = talk_thread();
  if (talk != NULL)
    talk->_SleepEvent->unpark();

  while(talk_thread() != NULL) {
    // This wait should make safepoint checks, wait without a timeout,
    // and wait as a suspend-equivalent condition.
    //
    // Note: If the FlatProfiler is running, then this thread is waiting
    // for the WatcherThread to terminate and the WatcherThread, via the
    // FlatProfiler task, is waiting for the external suspend request on
    // this thread to complete. wait_for_ext_suspend_completion() will
    // eventually timeout, but that takes time. Making this wait a
    // suspend-equivalent condition solves that timeout problem.
    //
    Terminator_lock->wait(!Mutex::_no_safepoint_check_flag, 0,
                          Mutex::_as_suspend_equivalent_flag);
  }
}

void TalkThread::print_on(outputStream* st) const {
  st->print("\"%s\" ", name());
  Thread::print_on(st);
  st->cr();
}
