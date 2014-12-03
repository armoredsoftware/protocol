#include "precompiled.hpp"
#include "runtime/thread.hpp"
#include "runtime/vm_operations.hpp"

#include "jr_custom_classes/stackWatcher.hpp"

//jlong last_time = -1;

bool VM_Our_CallStackWatcher::allow_nested_vm_operations() const { 
  return false;
}

void mark_time(methodOop moop) {
  HandleMark hm;
  methodHandle m = moop;
  if (m->is_on_stack()) {
    //tty->print_cr("Marking Time");
    m->note_seen_on_stack();
  }
}

int num_deopts = 0;
void deopt_aggressively(nmethod* nm) {
  //if (num_deopts++ < 1000) {
    nm->mark_for_deoptimization();
    //}
}

void VM_Our_CallStackWatcher::doit() {
  //Threads::our_stack_sweeper();
  /*{ MutexLockerEx mu(CodeCache_lock, Mutex::_no_safepoint_check_flag);
    //CodeCache::nmethods_do(&StackWatcher::sweep_methods);
    StackWatcher::watched_methods_do(&StackWatcher::sweep_methods);
    }*/

  // Measure the size of the collective size of nmethods in the code cache
  //CodeCache::nmethods_do(&mark_time);
  { MutexLockerEx mu(CodeCache_lock, Mutex::_no_safepoint_check_flag);
    CodeCache::nmethods_do(&StackWatcher::sweep_methods);
    //num_deopts = 0;
    //CodeCache::nmethods_do(&deopt_aggressively);
    //StackWatcher::did_mark = true;
      //StackWatcher::watched_methods_do(&StackWatcher::sweep_methods);
      //nmethod::watched_methods_do(&StackWatcher::sweep_methods);
      //SystemDictionary::methods_do(&StackWatcher::sweep_methods);
  }
  //SystemDictionary::methods_do(&mark_time);
  /*{ MutexLockerEx mu(CodeCache_lock, Mutex::_no_safepoint_check_flag);
    nmethod::cumulative_size = 0;
    StackWatcher::not_entrants_seen = 0;
    CodeCache::nmethods_do(&nmethod::measure_size);
    if (JRVerbose) {
      tty->print_cr("nmethods size: %12d - %5d", nmethod::cumulative_size, StackWatcher::not_entrants_seen);
    }
  }
  if (StackWatcher::largest_size < nmethod::cumulative_size) {
    StackWatcher::largest_size = nmethod::cumulative_size;
    if (JRVerbose) {
      tty->print_cr("Largest size: %12d", StackWatcher::largest_size);
    }
    }*/
}

void VM_Our_CallStackWatcher::doit_epilogue() {

}
