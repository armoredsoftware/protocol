
#include "precompiled.hpp"
#include "code/nmethod.hpp"
#include "code/codeCache.hpp"
#include "runtime/task.hpp"

#include "jr_custom_classes/methodCheckIn.hpp"
#include "jr_custom_classes/gcPauser.hpp"

class MethodCheckInTask : public PeriodicTask {
  friend MethodCheckInHandler;
private:
  static bool did_mark;
  static jlong tick_time;
public:
  MethodCheckInTask() : PeriodicTask(JRMethodDeoptCheckInInterval) {}
  
  static void sweep_method(nmethod* nm);

  void task() {
    //HandleMark hm;

    // prepare variables
    did_mark = false;
    tick_time = os::javaTimeMillis();//os::javaTimeNanos();
    //tty->print_cr("%d", tick_time);
    
    // scan all nmethods and execute sweep method on them
    { MutexLockerEx mu(CodeCache_lock, Mutex::_no_safepoint_check_flag);
      CodeCache::nmethods_do(&MethodCheckInTask::sweep_method);
    }
    
    // deoptimize marked methods if necessary
    if (MethodCheckInTask::did_mark) {
      VM_Deoptimize op;
      VMThread::execute(&op);
      did_mark = false;
    }
  }
};

MethodCheckInTask* MethodCheckInHandler::_task;
bool MethodCheckInTask::did_mark = false;
jlong MethodCheckInTask::tick_time = 0;

// Looks at the status value and determines what should be done
// with the compiled method.
void MethodCheckInTask::sweep_method(nmethod* nm) {
  //if (Universe::heap()->is_gc_active()) return;
  //if (GCPauser::get_gc_running()) return;
  //GCPauser::set_pauser_running(true);
  
  //MutexLockerEx pl(Patching_lock, Mutex::_no_safepoint_check_flag);

  methodOop m = nm->method();
  //address s = nm->get_our_compile_run_status_addr();
  //nmethodLocker nml(nm);
  MethodCheckInStatus* mcis = nm->get_our_compile_run_status();
  unsigned char s = mcis->get_value();
  bool should_check_mark = false;

  /*if ((*s & MethodCheckInHandler::not_compiled) > 0) {
    return;
  }
 
  if ((*s & MethodCheckInHandler::compile_not_used) > 0) {
    return;
  }
  
  if ((*s & MethodCheckInHandler::compile_active) > 0) {
    if (Universe::heap()->is_gc_active()) return;
    m->note_seen_on_stack();
    return;
  }

  if ((*s & MethodCheckInHandler::compile_seen_recent) > 0) {
    if (Universe::heap()->is_gc_active()) return;
    m->note_seen_on_stack();
    unsigned char k = *s;
    *s ^= MethodCheckInHandler::compile_seen_recent;
    return;
  }
  */

  // If the method has not been compiled do nothing -- nothing should reach here
  if ((s & ((unsigned char) MethodCheckInHandler::not_compiled)) > 0) {
    ShouldNotReachHere();
    //GCPauser::set_pauser_running(false);
    return;
  }

  // If the compiled method has not been used then do nothing
  if ((s & ((unsigned char) MethodCheckInHandler::compile_not_used)) > 0) {
    //GCPauser::set_pauser_running(false);
    return;
  }

  // If the compiled method has checked in but not out -- long lived methods
  if ((s & ((unsigned char) MethodCheckInHandler::compile_active)) > 0) {
    if (Universe::heap()->is_gc_active()) return;
    // make note of seeing the method
    m->note_seen_on_stack();
    //GCPauser::set_pauser_running(false);
    return;
  }

  // If the compile has recently checked in and back out -- short lived methods
  if ((s & ((unsigned char) MethodCheckInHandler::compile_seen_recent)) > 0) {
    if (Universe::heap()->is_gc_active()) return;
    // make note of seeing the method
    m->note_seen_on_stack();

    // toggle this bit
    mcis->set_value(s ^ ((unsigned char) MethodCheckInHandler::compile_seen_recent));
    //GCPauser::set_pauser_running(false);
    return;
  }

  //assert(mcis->get_value() == 0, "Invalid bit state");
  
  // We reach here if all of the conditions above have failed at the
  // time they were checked 
  //
  // Note: mcis can be changed since the last time the conditions were
  //       checked and therefore it is not always the case that 
  //       mcis->get_value() == 0 => true at this point.
  if (nm->is_native_method() || nm->is_marked_for_deoptimization()) { 
    // This method has already been marked or should not be marked
    return;
  }
  else if ((!Universe::heap()->is_gc_active()) && ((tick_time - m->get_last_time()) > JRMethodDeoptCheckInMarkTimer)) {
    if (!nm->is_osr_method()) {
      // Reset the counts so the compiler doesn't compulsively recompile
      // at next invocation of the method.
      m->invocation_counter()->reset();
      m->backedge_counter()->reset();
    }
    
    // Mark the method for deoptimization by NmethodSweeper
    nm->JR_mark_for_deoptimization();
    // ResourceMark rm;
    // tty->print_cr("Method: %s", m->name_and_sig_as_C_string());

    // Notify the periodic task to sweep the nmethods.
    did_mark = true;
  }

  //GCPauser::set_pauser_running(false);
}

void MethodCheckInHandler::engage() {
  _task = new MethodCheckInTask();
  _task->enroll();
}

void MethodCheckInHandler::disengage() {
  _task->disenroll();
  delete _task;
  _task = NULL;
}

jlong MethodCheckInHandler::current_tick() {
  return _task->tick_time;
}

