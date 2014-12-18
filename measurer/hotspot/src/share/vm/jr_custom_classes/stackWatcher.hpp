
#ifndef STACK_WATCHER_HPP
#define STACK_WATCHER_HPP

#include "memory/allocation.hpp"
#include "runtime/thread.hpp"
#include "code/nmethod.hpp"
#include "oops/oop.hpp"
//#ifdef TARGET_OS_FAMILY_linux
# include <setjmp.h>
//#endif

class StackWatcherTask;

typedef struct MethodWatchNode {
  nmethod* nm;
  
  MethodWatchNode* next;
} MethodWatchNode;

class StackWatcher: AllStatic
{
  friend class StackWatcherTask;
  friend class VM_Our_CallStackWatcher;
  friend class OurStackWatcherDeoptThread;
 private:
  //static JavaThread* stack_watcher_thread_ptr;

  static MethodWatchNode* head;
  static MethodWatchNode* tail;

  static volatile bool running;
  static bool did_mark;
  static int  sleep_interval;
  static int  scan_depth;
  static jlong mark_timer;
  static jlong tick_time;
  static jlong largest_size;
  static vframe* garbage_vframe;
  static pthread_t SWDT_pthread_ptr;
  static JavaThread* working_thread;
  static bool fault_detected;

  //static void stack_watcher_main();
  static void sweep_methods(nmethod* nm);
  static void sweep_methods(methodOop nm);
  //static void sweep_methods(MethodWatchNode* prev, MethodWatchNode* cur);

  // For use with periodic task implementation
  static StackWatcherTask* _task;

  //static void watched_methods_do(void (*func)(MethodWatchNode* prev, MethodWatchNode* cur));
  static void print_list();

  static void stack_watcher_main();
  
 public:
  static sigjmp_buf exception_env;
  static int not_entrants_seen;

  static void delete_garbage_vframe();

  static pthread_t get_pthread() { return SWDT_pthread_ptr; }
  static bool did_mark_nmethods() { return did_mark; }
  //static bool is_running() { return running; }
  static jlong current_tick() { return tick_time; }

  static JavaThread* get_working_thread() { return working_thread; }
  //static vframe* get_working_vframe() { return working_vframe; }
  //static vframe* get_working_vframe_sender() { return working_vframe_sender; }
  /*static void set_safe_working_vframe(vframe* safe_vframe) {
    working_vframe = safe_vframe;
    fault_detected = true;
    }*/

  static void set_scan_depth(int depth) {
    scan_depth = depth;
  }

  static jlong get_largest_size() { return largest_size; }

  static JavaThread* start_stack_watcher();
  static void end_stack_watcher();

  static void sweep_stack(JavaThread* thread);

  // For use with periodic task implementation
  static void engage(int task_interval);
  static void disengage();

};

#endif
