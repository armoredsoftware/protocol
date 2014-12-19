//#include <cstdlib>

#include "precompiled.hpp"

#include "jr_custom_classes/papiThreadShadow.hpp"
#include "jr_custom_classes/papiManager.hpp"
#include "jr_custom_classes/papi_headers/papi.h"

#include "memory/allocation.hpp"

// Enabling this define breaks profiled methods into sections if there
// is a call inside that method to another profiled method. This gives
// us a linear picture as to what is going on with the data. Otherwise
// our counters accumulate onto the previous stack node on method
// return.
#define METHOD_PARTS

oper_count PapiThreadShadow::operation_count = 0;

PapiThreadShadow* PapiThreadShadow::head = NULL;
PapiThreadShadow* PapiThreadShadow::tail = NULL;

bool PapiThreadShadow::should_collect = false;

char* unknown_method = NULL;

pthread_mutex_t mymutex = PTHREAD_MUTEX_INITIALIZER;

// Report PAPI errors and crash the VM
#define PAPI_ERROR_REPORTING(msg, errval) \
  { tty->print_cr("%s (line %d): %s: %s", __FILE__, __LINE__, msg, PAPI::strerror(errval)); assert(false, "PAPI: Fatal Error"); }

PapiNode::PapiNode() : next(NULL) {
  const char* unknown = "[UNKNOWN]";
  
  counters = new papi_cntr[PAPI::get_num_events()];
  
  for (int i = 0; i < PAPI::get_num_events(); i++) {
    counters[i] = 0;
  }

  if (unknown_method == NULL) {
    unknown_method = new char[strlen(unknown)+1];
    strcpy(unknown_method, unknown);
  }

  m_name = unknown_method;
  part_num = 0;
}

PapiNode::PapiNode(char* method_name) : next(NULL) {
  counters = new papi_cntr[PAPI::get_num_events()];
  
  for (int i = 0; i < PAPI::get_num_events(); i++) {
    counters[i] = 0;
  }

  m_name = method_name;
  part_num = 0;
}

// Copy node and take it's counters and replace them with zeroed
// counters
PapiNode::PapiNode(PapiNode& node) : next(NULL) {
  papi_cntr* other_counters = new papi_cntr[PAPI::get_num_events()];
  counters = node.counters;
  node.counters = other_counters;

  for (int i = 0; i < PAPI::get_num_events(); i++) {
    other_counters[i] = 0;
  }

  entry = node.entry;
  exit = 0;

  m_name = node.m_name;
  part_num = node.part_num++;
} 

PapiNode::~PapiNode() {
  delete [] counters;
}

// Create a thread shadow
PapiThreadShadow::PapiThreadShadow() : next(NULL), head_stack(NULL), head_queue(NULL), tail_queue(NULL), event_set(PAPI_NULL), empty(true), is_collecting(false), start_count(0), stop_count(0), java_thread(NULL), totals(NULL), thread_name(NULL), thread_id(0) {
  // Probably should synchronize this
  pthread_mutex_lock(&mymutex);
  if (tail == NULL) {
    head = this;
    tail = this;
    prev = NULL;
  }
  else {
    tail->next = this;
    prev = tail;
    tail = this;
  }
  pthread_mutex_unlock(&mymutex);

  start_count = operation_count;
}

// Destroy the thread shadow
PapiThreadShadow::~PapiThreadShadow() {
  assert(!is_collecting, "Cannot delete PapiThreadShadow while still collecting data");

  PapiNode* temp;

  // Empty the stack
  while (head_stack != NULL) {
    temp = head_stack;
    head_stack = head_stack->next;
    delete temp;
  }

  // Empty the queue
  while (head_queue != NULL) {
    temp = head_queue;
    head_queue = head_queue->next;
    delete temp;
  }

  delete [] totals;
  
  stop_collection();
  
  // TODO: Verify this does not need to be synchronized
  if (prev != NULL)
    prev->next = next;
  if (next != NULL)
    next->prev = prev;
}

// Stop collecting data
int PapiThreadShadow::stop_collection() {
  int errval;
  if (is_collecting) {
    // Pop any remaining stack nodes
    while (head_stack != NULL) {
      pop_method_thread();
    }

    if ((errval = PAPI::stop(event_set, NULL)) != PAPI_OK)
      PAPI_ERROR_REPORTING("Stop Failed", errval);

    if ((errval = PAPI::cleanup_eventset(event_set)) != PAPI_OK)
      PAPI_ERROR_REPORTING("Eventset Cleanup Failed", errval);

    if ((errval = PAPI::destroy_eventset(&event_set)) != PAPI_OK)
      PAPI_ERROR_REPORTING("Eventset Destroy Failed", errval);

    //if ((errval = PAPI::unregister_thread()) != PAPI_OK)
    //  PAPI_ERROR_REPORTING("Unregister Thread Failed", errval);

    is_collecting = false;

    return 0;
  }
  
  return 1;
}

// Start collecting data on a thread
int PapiThreadShadow::profile_thread(JavaThread* java_thread) {
  PapiThreadShadow* pts;
  int errval;
  bool thread_reused = false;
  pid_t tid = java_thread->osthread()->thread_id();
  
  operation_count++;

  // Check if this thread is already registered under a different name
  pts = head;
  while (pts != NULL) {
    if (pts->thread_id == tid) {
      thread_reused = true;
      break;
    }
    pts = pts->next;
  }

  // New threads need initializing otherwise skip this
  if (!thread_reused) {
    // Tells papi to watch the thread
    if ((errval = PAPI::register_thread()) != PAPI_OK)
      PAPI_ERROR_REPORTING("Register Thread Failed", errval);
  
    pts = new PapiThreadShadow();

    // Setup the eventset
    if ((errval = PAPI::create_eventset(&pts->event_set)) != PAPI_OK)
      PAPI_ERROR_REPORTING("Eventset Create Failed", errval);
  
    // Add events
    for (int i = 0; i < PAPI::get_num_events(); i++) {
      if ((errval = PAPI::add_event(pts->event_set, PAPI::Events[i])) != PAPI_OK)
	PAPI_ERROR_REPORTING("Event Add Failed", errval);
    }

    // Start counting events
    if ((errval = PAPI::start(pts->event_set)) != PAPI_OK)
      PAPI_ERROR_REPORTING("Start Failed", errval);
  }
  else {
    tty->print("%s ---> ", pts->thread_name);
    delete [] pts->thread_name;
  }
  
  // Assign java_thread to pts
  pts->java_thread = java_thread;

  // Assign pts to java_thread
  java_thread->set_papi_thread_shadow(pts);
  
  // Thread shadow will exist even after the thread is
  // destroyed. Therefore, information about the thread needs to be
  // collected now.

  // Copy thread name
  { ResourceMark rm;
    pts->thread_name = new char[strlen(java_thread->get_thread_name())+1];
    strcpy(pts->thread_name, java_thread->get_thread_name());
  }

  if (thread_reused) {
    tty->print_cr("%s", pts->thread_name);
  }

  // Set thread_id
  pts->thread_id = tid;

  // Set total counts
  pts->totals = new papi_cntr[PAPI::get_num_events()];
  for (int i = 0; i < PAPI::get_num_events(); i++) {
    pts->totals[i] = 0;
  }

  // Set states
  pts->is_collecting = true;
  pts->empty = true;

  return 0;
}

// Stop profiling the specified thread
int PapiThreadShadow::stop_profile_thread(JavaThread* java_thread) {
  PapiThreadShadow* pts = java_thread->get_papi_thread_shadow();
  pts->stop_collection();
  //java_thread->set_papi_thread_shadow(NULL);
  //java_thread = NULL;

  return 0;
}

// Stop profiling all threads
int PapiThreadShadow::stop_profile_all_threads() {
  PapiThreadShadow* temp = head;
  
  while (temp != NULL) {
    temp->stop_collection();
    temp = temp->next;
  }
  
  return 0;
}

// Delete shadow threads and all their content
int PapiThreadShadow::destory_shadow_threads() {
  PapiThreadShadow* temp;
  
  while (head != NULL) {
    temp = head;
    head = head->next;
    delete temp;
  }

  return 0;
}

// Thread specific method entry data collector
void PapiThreadShadow::push_method_thread(char* method_name) {
  assert(java_thread == JavaThread::current(), "just checking");
  assert(is_collecting, "Must be collecting data");
  PapiNode* temp;
  int errval;
  papi_cntr temp_cntr[PAPI::get_num_events()];

  tty->print_cr("REACHED PUSH");  

  // Handle stack previously empty
  if (empty) {
    if ((errval = PAPI::reset(event_set)) != PAPI_OK)
      PAPI_ERROR_REPORTING("Reset Failed", errval);

    empty = false;
  }
  else {
    // Store the number counts in prev function
    if ((errval = PAPI::read(event_set, temp_cntr)) != PAPI_OK)
      PAPI_ERROR_REPORTING("Accum Failed", errval);

    // Accumulate totals and head_stack counters for all events
    for (int i = 0; i < PAPI::get_num_events(); i++) {
      totals[i] += temp_cntr[i];
      head_stack->counters[i] += temp_cntr[i];
    }

#ifdef METHOD_PARTS
    // Get data from this method and reset our counters
    temp = new PapiNode(*head_stack);
    temp->exit = operation_count;

    // Add the node to the queue
    if (tail_queue != NULL) {
      tail_queue->next = temp;
      tail_queue = temp;
    }
    else {
      head_queue = tail_queue = temp;
    }
#endif
  }

  // Insert the node
  temp = new PapiNode(method_name);
  temp->next = head_stack;
  head_stack = temp;
  head_stack->entry = operation_count;

  // Ignore counts in this function reset papi's counters
  if ((errval = PAPI::reset(event_set)) != PAPI_OK)
    PAPI_ERROR_REPORTING("Reset Failed", errval);
}

// Thread specific method exit data collector
void PapiThreadShadow::pop_method_thread() {
  if (empty) { 
    tty->print_cr("PAPI Warning: Tried to pop empty stack");
    return;
  }

  assert(is_collecting, "Must be collecting data");

  PapiNode* temp;
  int errval;
  int num_events = PAPI::get_num_events();
  papi_cntr temp_cntr[num_events];

  tty->print_cr("REACHED POP");

  // Store the counters first thing. 
  if ((errval = PAPI::read(event_set, temp_cntr)) != PAPI_OK)
    PAPI_ERROR_REPORTING("Accum Failed", errval);

  head_stack->exit = operation_count;
  
  // Pop the node.
  temp = head_stack;
  head_stack = head_stack->next;
  temp->next = NULL;
  
#ifdef METHOD_PARTS
  // Store the method fragment entry for the newly exposed head method
  if (head_stack != NULL)
    head_stack->entry = operation_count;  
#endif

  // Add the node to the queue
  if (tail_queue != NULL) {
    tail_queue->next = temp;
    tail_queue = temp;
  }
  else {
    head_queue = tail_queue = temp;
  }
  
  // Accumulate counts into the the total counts and into the popped
  // node's counters
  for (int i = 0; i < num_events; i++) {
    temp->counters[i] += temp_cntr[i];
    totals[i] += temp_cntr[i];
  }

  if (head_stack != NULL) {
#ifndef METHOD_PARTS
    // Add the popped node counts to the next node.
    for (int i = 0; i < num_events; i++) {
      head_stack->counters[i] += temp->counters[i];
    }
#endif
  }
  else {
    // Empty stack
    stop_count = operation_count;
    empty = true;
  }
  
  // Ignore counts in this funcion
  // This might be too expensive
  if ((errval = PAPI::reset(event_set)) != PAPI_OK)
    PAPI_ERROR_REPORTING("Reset Failed", errval);
}

// TODO: Implement

// Output data from queues during runtime from a separate thread. This
// will hopefully prevent us from running out of memory from queues
// getting too big which is a problem for long running programs
/*void PapiThreadShadow::output_data_concurrent(FILE* out) {
  PapiNode* tail_node = tail_queue;
  PapiNode* temp;

  while (head_queue != tail_node) {
    if (head_queue->counters[0] > 0) {
      // Print method name
      
      if (head_queue->m_name != NULL) {
#ifdef METHOD_PARTS
	fprintf(out, "$ %s+%d", head_queue->m_name, head_queue->part_num);
#else
	fprintf(out, "$ %s", head_queue->m_name);
#endif
      }
      else
	fprintf(out, "$ <NULL>");
    
      // Print stack entry and exit counters
      fprintf(out, ",%llu,%llu", temp->entry, temp->exit);

      // Print event counters
      for (int i = 0; i < PAPI::get_num_events(); i++) {
	fprintf(out, ",%llu", temp->counters[i]);
      }
      fprintf(out, "\n");
    }
    
    temp = head_queue;
    
    
    delete temp;
    temp = temp->next;
  }
  }*/

// Output data from this thread shadow to a file at the end of a
// program's execution
void PapiThreadShadow::output_data_thread(FILE* out) {
  //assert(is_collecting, "Cannot be collecting at this point");
  PapiNode* temp = head_queue;
  char buf[PAPI_MAX_STR_LEN];
  //unsigned int consec = 1;

  // Output thread info
  fprintf(out, "~ Thread name: %s\n", thread_name);
  fprintf(out, "~ Thread id: 0x%x\n", thread_id);

  // Write table headders
  fprintf(out, "@ Method Name,Stack Entry,Stack Exit");
  for (int i = 0; i < PAPI::get_num_events(); i++) {
    PAPI::event_code_to_name(PAPI::Events[i], buf);
    fprintf(out, ", %s", buf);
  }
  fprintf(out, "\n");

  // Write thread shadow total counts to a line
  fprintf(out, "* TOTALS,%llu,%llu", start_count, stop_count);
  for (int i = 0; i < PAPI::get_num_events(); i++) {
    fprintf(out, ",%llu", totals[i]);
  }
  fprintf(out, "\n");

  while (temp != NULL) {
    if (temp->counters[0] > 0) {
      // Print method name
      if (temp->m_name != NULL) {
#ifdef METHOD_PARTS
	fprintf(out, "$ %s+%d", temp->m_name, temp->part_num);
#else
	fprintf(out, "$ %s", temp->m_name);
#endif
      }
      else
	fprintf(out, "$ <NULL>");
    
      // Print stack entry and exit counters
      fprintf(out, ",%llu,%llu", temp->entry, temp->exit);

      // Print event counters
      for (int i = 0; i < PAPI::get_num_events(); i++) {
	fprintf(out, ",%llu", temp->counters[i]);
      }
      fprintf(out, "\n");
    }
    
    temp = temp->next;
  }

  // Output end of this thread shadow's data. Anything after this is
  // from a different thread shadow or EOF.
  fprintf(out, "\n~ ********************************************************************************\n\n");
}

/// Implementation Problems: Possible FIXME but not too critical ///
//
// Theoretically, for each call to push_method there should be a
// pop_method. This is not the case in practice for several possible
// reasons:
//   1) The call to PapiThreadShadow::enable_collection occurs while
//   the program stack is not empty resulting in extra calls to
//   pop_method.
//
//   2) OSR Methods are also being profiled and not registering
//   entry. I am unsure how OSR methods are constructed, however it
//   seems logical that this may be a possible problem.
//
// Reason 1 seemed like the most correct since this problem did not
// seem to occur before adding "enable_collection." However, it does
// fail to explain why this problem reoccurs across several
// iterations. The OSR reasoning also fails to explain this unless
// methods are still being compiled in later iterations.

// Static function easily callable from JIT compiled
// assembly. Determines which thread shadow we need to record data for and
// calls push_method_thread on that thread shadow.
void PapiThreadShadow::push_method(char* method_name) {
  if (Thread::current()->is_Java_thread()) {
    JavaThread* jt = JavaThread::current();
    PapiThreadShadow* pts = jt->get_papi_thread_shadow();
    if (pts == NULL && should_collect) {
      PapiThreadShadow::profile_thread(jt);
      pts = jt->get_papi_thread_shadow();
    }
    else if (pts == NULL || !pts->is_collecting) {
      return;
    }

    operation_count++;
    pts->push_method_thread(method_name);    
  }
  else {
    ShouldNotReachHere();
  }
}

// Static function easily callable from JIT compiled
// assembly. Determines which thread shadow we need to record data for
// and calls pop_method_thread on that thread shadow.
void PapiThreadShadow::pop_method() {
  if (Thread::current()->is_Java_thread()) {
    PapiThreadShadow* pts = JavaThread::current()->get_papi_thread_shadow();
    if (pts == NULL)
      return;
    else if (pts->is_collecting) {
      operation_count++;
      pts->pop_method_thread();
    }
  }
  else {
    ShouldNotReachHere();
  }
}

// Calls the output_data_thread function for each thread shadow
void PapiThreadShadow::output_data(FILE* out) {
  PapiThreadShadow* temp = head;

printf("===Thread Shadows===\n");
  if (out == NULL)
    return;
int i = 0;
  while (temp != NULL) {
printf("Thread %d {\n", i);
    temp->output_data_thread(out);
i++;
    temp = temp->next;
printf("}\n", i);
  }
printf("============\n");
  
  if (out != stdout)
    tty->print_cr("\nPAPI: Wrote data to %s", PAPIOutputFile);
}
