#include "precompiled.hpp"
#include "runtime/thread.hpp"
#include "runtime/frame.hpp"
#include "code/nmethod.hpp"
#include "code/codeCache.hpp"
#include "runtime/task.hpp"
//#include "utilities/exceptions.hpp"

#include "jr_custom_classes/methodGatherer.hpp"
#include "jr_custom_classes/stackWatcher.hpp"
#include "jr_custom_classes/methodCheckIn.hpp"

#define DETECT_FAULT if (fault_detected) { fault_detected = false; tty->print_cr("breaking?"); break; } //else { tty->print_cr("No fault"); } 

MethodWatchNode* StackWatcher::head = NULL;
MethodWatchNode* StackWatcher::tail = NULL;

volatile bool  StackWatcher::running = false;
bool  StackWatcher::did_mark = false;
int   StackWatcher::sleep_interval;
int   StackWatcher::scan_depth = 1000;
jlong StackWatcher::mark_timer;
jlong StackWatcher::tick_time;
int StackWatcher::not_entrants_seen = 0;
vframe* StackWatcher::garbage_vframe = NULL;
bool StackWatcher::fault_detected = false;

StackWatcherTask* StackWatcher::_task = NULL;
JavaThread* StackWatcher::working_thread = NULL;
pthread_t StackWatcher::SWDT_pthread_ptr = 0;

sigjmp_buf StackWatcher::exception_env;

jlong StackWatcher::largest_size = 0;

void print_flags(nmethod* nm) {
  //tty->print_cr("%x",(unsigned int)*(nm->method()->our_compile_run_status));
}

class StackWatcherTask : public PeriodicTask {
 public:
  StackWatcherTask(int task_interval) : PeriodicTask(task_interval) {}
  void task() {
    if (MethodCallGatherer::_is_waiting_on_gc) return;
    //{ MutexLockerEx mu(CodeCache_lock, Mutex::_no_safepoint_check_flag);
    //  CodeCache::nmethods_do(print_flags);
    //}
    //MethodCallGatherer::_is_doing_collection = true;

    HandleMark hm;

    if (!StackWatcher::running) {
      StackWatcher::running = true;
    }
    else {
      return;
    }
    
    //jlong new_time = os::javaTimeNanos();
    //tty->print_cr(" Time: %d", (int)((new_time - StackWatcher::tick_time)/1000000));
    //StackWatcher::tick_time = new_time;
    StackWatcher::tick_time = os::javaTimeNanos();
    
    VM_Our_CallStackWatcher op;
    VMThread::execute(&op);
    //tty->print_cr("\t entering %d", SafepointSynchronize::is_at_safepoint());
    { MutexLockerEx mu(CodeCache_lock, Mutex::_no_safepoint_check_flag);
      CodeCache::nmethods_do(&StackWatcher::sweep_methods);
      //StackWatcher::watched_methods_do(&StackWatcher::sweep_methods);
      //nmethod::watched_methods_do(&StackWatcher::sweep_methods);
      //SystemDictionary::methods_do(&StackWatcher::sweep_methods);
    }
    //tty->print_cr("\t exiting %d", SafepointSynchronize::is_at_safepoint());
    if (StackWatcher::did_mark) {
      VM_Deoptimize op2;
      VMThread::execute(&op2);
      StackWatcher::did_mark = false;
    }
    
    StackWatcher::running = false;
    //MethodCallGatherer::_is_doing_collection = false;
  }
};

void StackWatcher::stack_watcher_main() {
  if (MethodCallGatherer::_is_waiting_on_gc) return;
  HandleMark hm;

  StackWatcher::tick_time = os::javaTimeNanos();

  Threads::our_stack_sweeper();
}

// We are using this one
void StackWatcher::sweep_methods(nmethod* nm) {
  /*if (Universe::heap()->is_gc_active()) return;
    if (nm->method() == NULL) return;*/
  //if (!nm->method()->being_watched()) return;
  //if (!nm->method()->is_oop()) return;
  if (MethodCallGatherer::_is_waiting_on_gc) return;
  methodHandle m = nm->method();
  char status_flags = m->our_compile_run_status->get_value();
  
  //tty->print_cr("%d", (0b111 & status_flags));
  if (status_flags & 0b100 == 0b100) {
    m->note_seen_on_stack();
  }
  if (status_flags & 0b010 == 0b010) {
    m->note_seen_on_stack();
    
    status_flags = status_flags & 0b101;
  } 
  else if (status_flags & 0b001 == 0b001) {
    m->note_seen_on_stack();
  }

  if (nm->is_native_method() || nm->is_marked_for_deoptimization()) {
    return;
  }
  else if (tick_time - m->get_last_time() > mark_timer && m->being_watched()) {
    //tty->print_cr("SWEPT METHOD MARKING");
    if (!nm->is_osr_method()) {
      m->invocation_counter()->reset();
      m->backedge_counter()->reset();
    }
    nm->mark_for_deoptimization();
    did_mark = true;
  }
}

void StackWatcher::sweep_methods(methodOop moop) {
  methodHandle m = moop;
  nmethod* nm = m->code();
  /*if (m->code() != NULL) {
    nmethodLocker nml(m->code());
    nm = m->code();
  }
  else {
    return;
  }*/

  if (nm->is_native_method() || nm->is_marked_for_deoptimization()) {
    return;
  }
  else if (tick_time - m->get_last_time() > mark_timer && m->being_watched()) {
    //tty->print_cr("SWEPT METHOD MARKING");
    if (!nm->is_osr_method()) {
      m->invocation_counter()->reset();
      m->backedge_counter()->reset();
    }
    nm->mark_for_deoptimization();
    did_mark = true;
  }
}

/*void StackWatcher::sweep_methods(MethodWatchNode* prev, MethodWatchNode* cur) {
  nmethod* nm = cur->nm;
  tty->print_cr("destroy");
  methodHandle m = nm->method();
  if (m->get_last_time() == -1 || nm->is_native_method() || nm->is_marked_for_deoptimization()) {
    return;
  }
  else if (tick_time - m->get_last_time() > mark_timer) {
    nm->mark_for_deoptimization();
    m->invocation_counter()->reset();
    m->backedge_counter()->reset();
    m->note_deoptimization();
    remove(prev, cur);
    did_mark = true;
  }
  }*/

void StackWatcher::sweep_stack(JavaThread* thread) {
  // if the stack is empty then running this method is dangerous so return
  if (!thread->has_last_Java_frame()) {
    return;
  }

  //TODO: Perhaps make separate variables for this gc blocker rather than
  //  reusing MethodCallGatherer's blocking mechanism
  //MethodCallGatherer::_is_doing_collection = true;
  //while (MethodCallGatherer::_is_waiting_on_gc)
  //  usleep(100);

  ResourceMark rm;
  RegisterMap reg_map(thread);
  
  working_thread = thread;

  // Scan all of the vframes on the thread's stack
  // A vframe represents a method on the stack
  int safety = 0;
  int jvframes = 0;
  //assert(garbage_vframe == NULL, "Memory leak: Garbage vframe was not deleted");

  for (vframe* f = thread->last_java_vframe(&reg_map); f; f = f->sender()) {
    if (garbage_vframe != NULL) {
      //delete_garbage_vframe();
    }

    if (safety++ > scan_depth) {
      tty->print_cr("JR WARNING: Massive stack detected, aborting current stack scan");
      return;
    }

    //tty->print_cr("frame %d", safety);

    if (f->is_java_frame()) {      
      //methodHandle m = javaVFrame::cast(f)->method();
      if (f->cb()->is_nmethod()) {
	nmethod* nm = f->nm();//m->code();
	if (nm != NULL) {
	  /*if (!nm->method()->being_watched()) {
	    nm->add();
	  }*/
	  nm->method()->note_seen_on_stack();

	  ++jvframes;

	  //tty->print_cr("0x%x - %d - %s", thread, jvframes, nm->method_name_holder.get_cstring());
	  
	  // Abort after we've searched the specified number of frames 
	  if (jvframes >= 1) {
	    return;
	  }
	}
      }
    }

    garbage_vframe = f;
  }
  //MethodCallGatherer::_is_doing_collection = false;
}

JavaThread* StackWatcher::start_stack_watcher() {
  /*JavaThread* stack_watcher_thread = NULL;

  running = true;
  sleep_interval = JRMethodDeoptStackWatcherInterval * K;
  mark_timer = JRMethodDeoptStackWatcherMarkTimer * K;
  
  klassOop k =
    SystemDictionary::resolve_or_fail(vmSymbols::java_lang_Thread(),
                                      true, CHECK_0);
  instanceKlassHandle klass (THREAD, k);
  instanceHandle thread_oop = klass->allocate_instance_handle(CHECK_0);
  Handle string = java_lang_String::create_from_str("Stack Watcher", CHECK_0);

  // Initialize thread_oop to put it into the system threadGroup
  Handle thread_group (THREAD,  Universe::system_thread_group());
  JavaValue result(T_VOID);
  JavaCalls::call_special(&result, thread_oop,
                       klass,
                       vmSymbols::object_initializer_name(),
                       vmSymbols::threadgroup_string_void_signature(),
                       thread_group,
                       string,
                       CHECK_0);

  //os::signal_init_pd();

  { MutexLocker mu(Threads_lock);
    JavaThread* stack_watcher_thread = new JavaThread(&stack_watcher_main);

    // At this point it may be possible that no osthread was created for the
    // JavaThread due to lack of memory. We would have to throw an exception
    // in that case. However, since this must work and we do not allow
    // exceptions anyway, check and abort if this fails.
    if (stack_watcher_thread == NULL || stack_watcher_thread->osthread() == NULL) {
      vm_exit_during_initialization("java.lang.OutOfMemoryError",
				    "unable to create new native thread");
    }

    java_lang_Thread::set_thread(thread_oop(), stack_watcher_thread);
    java_lang_Thread::set_priority(thread_oop(), NearMaxPriority);
    java_lang_Thread::set_daemon(thread_oop());

    stack_watcher_thread->set_threadObj(thread_oop());
    Threads::add(stack_watcher_thread);
    Thread::start(stack_watcher_thread);
    }*/
  /*{ MutexLocker mu(Threads_lock);
    running_thread = new CompilerThread(&stack_watcher_main);
    }*/
}

void StackWatcher::end_stack_watcher() {
  running = false;
}

void StackWatcher::engage(int task_interval) {
  mark_timer = (jlong)JRMethodDeoptStackWatcherMarkTimer * MICROUNITS;
  _task = new StackWatcherTask(task_interval);
  _task->enroll();
}

void StackWatcher::disengage() {
  _task->disenroll();
  delete _task;
  _task = NULL;
}

void StackWatcher::print_list() {
  MethodWatchNode* cur = head;
  while (cur != NULL) {
    tty->print(INTPTR_FORMAT"->", cur->nm);
    cur = cur->next;
  }
  tty->print_cr("NULL");
}

void StackWatcher::delete_garbage_vframe() {
  delete garbage_vframe;
  garbage_vframe = NULL;
}
