#ifndef PAPI_THREAD_SHADOW_HPP
#define PAPI_THREAD_SHADOW_HPP

#include "memory/allocation.hpp"
#include "jr_custom_classes/papiManager.hpp"
#include "jr_custom_classes/papi_headers/papi.h"

typedef unsigned long long oper_count;

typedef struct PapiNode : public CHeapObj{
  char* m_name;

  int part_num;

  papi_cntr* counters;
  oper_count entry;
  oper_count exit;

  struct PapiNode* next;
  
  PapiNode();
  PapiNode(char* method_name);
  PapiNode(PapiNode& node);
  ~PapiNode();

} PapiNode;

class PapiThreadShadow : public CHeapObj{
  friend class PAPI;
private:
  // Counts PapiThreadShadow operations to give a better idea of when
  // an operation happens relative to other operations
  static oper_count operation_count;

  // Set after the first benchmark iteration by interpreter call to
  // InterpreterRuntime::isBetweenInteractions
  static bool should_collect;

  // Head and tail of PapiThreadShadow list
  static PapiThreadShadow* head;
  static PapiThreadShadow* tail;

  // The Java thread this shadow stack corresponds to
  JavaThread* java_thread;

  // JavaThread properties
  char* thread_name;
  pid_t thread_id;
  
  // Tracks life span of the thread
  oper_count start_count;
  oper_count stop_count;

  // Total counts for all events
  papi_cntr* totals;

  // Papi eventset for the thread
  int event_set;

  // State variables
  bool empty;
  bool is_collecting;

  // Top of the shadow stack
  PapiNode* head_stack;

  // Queue for PapiNodes after they've been popped off the stack
  PapiNode* head_queue;
  PapiNode* tail_queue;

  // Link the thread shadows
  PapiThreadShadow* next;
  PapiThreadShadow* prev;

  PapiThreadShadow();
  ~PapiThreadShadow();

  int stop_collection();

public:
  static oper_count get_operation_count() { return operation_count; }
  static void enable_collection() { should_collect = true; }

  static int profile_thread(JavaThread* java_thread);
  static int stop_profile_thread(JavaThread* java_thread);
  static int stop_profile_all_threads();
  static int destory_shadow_threads();

  void push_method_thread(char* method_name);
  void pop_method_thread();

  void output_data_thread(FILE* out);

  // Call from assembly - resolves thread and calls the non-static
  // thread version of the function
  static void push_method(char* method_name);
  static void pop_method();

  // Output data for all PapiShadowThreads
  static void output_data(FILE* out);
};

#endif
