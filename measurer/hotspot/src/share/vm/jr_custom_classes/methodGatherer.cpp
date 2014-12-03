#include "precompiled.hpp"
#include "runtime/task.hpp"
#include "runtime/vmThread.hpp"
#include "runtime/timer.hpp"
#include "runtime/java.hpp"
#include "runtime/vm_operations.hpp"
#include "oops/methodOop.hpp"

#include "jr_custom_classes/methodCollector.hpp"
#include "jr_custom_classes/methodGatherer.hpp"

int MethodCallGatherer::_gathererTicks;
int MethodCallGatherer::_gathererEarlyTicks;
int MethodCallGatherer::_garbageCollectorTicks;
int MethodCallGatherer::_gathererTimeHistogram[PeriodicTask::max_interval];
int MethodCallGatherer::_visibleInterval;
volatile bool MethodCallGatherer::_is_waiting_on_gc;
volatile bool MethodCallGatherer::_is_doing_collection;
bool MethodCallGatherer::is_queued = false;
elapsedTimer MethodCallGatherer::_gathererTimer;

MethodCallGathererTask*  MethodCallGatherer::_task = NULL;

// Instructions for each tick
void MethodCallGathererTask::task() {
  if (_running) {
    // To avoid a deadlock with the garbage collector and collect_final_interval()
    // we just assume it's safe to collect the data if we're _waiting_to_finish.
    // Seems to not be a problem.
    if (_waiting_to_finish) {
      MethodCallGatherer::collect_nodes();
    }
    // If we're waiting on the GC full dump to finish then skip this collection,
    // maybe enqueue a vm_operation to cary it out at a later time, and note
    // the skip.
    else if (MethodCallGatherer::_is_waiting_on_gc) {
      if (JRMethodCountTraceUseVMOP && !MethodCallGatherer::is_queued) {
	MethodCallGatherer::is_queued = true;
	VM_Our_CallMethodCollector cmc;
	VMThread::execute(&cmc);
	//tty->print_cr("Using VM Operation");
      }
      
      MethodCallGatherer::_garbageCollectorTicks++;
    }
    // If we don't have a vm_operation queued then simply collect the data
    else if (!MethodCallGatherer::is_queued) {
      MethodCallGatherer::collect_nodes();
    }
  }
}

// Initialize the PeriodicTask and enroll it with the WatcherThread
void MethodCallGatherer::engage() { 
  _task = new MethodCallGathererTask(JRMethodCountTraceTaskInterval);
  _is_waiting_on_gc = false;
  _gathererTimer.start();
  _task->enroll();
  _task->_running = true;
}

// Remove this PeriodicTask from the WatcherThread
void MethodCallGatherer::disengage() {
  //print_our_final_method_invocation_histogram();
  //output_our_final_method_invocation_histogram();
  //print_gatherer_intervals();
  
  _task->disenroll();
  delete _task;
  _task = NULL;
}

// call the collection consistency printer and then output the data to a file
void MethodCallGatherer::print_statistics_and_output_data() {
  print_gatherer_intervals();

  tty->cr();
  tty->print_cr ("Check the above histogram and verify that the accuracy is acceptable.");
  if (!JRMethodCountTraceSupressOutput) {
    output_our_final_method_invocation_histogram();
  }
}

// ----- Spinning used - Probably needs improvement -----
// Spin the caller thread until the method collector has been called for the last time
void MethodCallGatherer::collect_final_interval() {
  _task->_waiting_to_finish = true;
  
  // This method is called near the end of the vm's execution and haults the exit procedure
  // so that we may see the final methods that were invoked before the exit began.
  // Sleep for a tenth of the target interval between each check so that it is not quite so
  // expensive to spin
  while (_task->_waiting_to_finish)
    usleep(_visibleInterval*100);
  
  _task->_running = false;
}

// prints the spread of the collection intervals
void MethodCallGatherer::print_gatherer_intervals() {
  if (JRMethodCountTrace) {
    tty->print_cr("** Time Between Print Intervals (ideal %5d) **", _visibleInterval);
    for (int i = 0; i < PeriodicTask::max_interval; i++) {
      int n = _gathererTimeHistogram[i];
      if (n > 0) {
	tty->print_cr("* %5d: %5d (%5.1f\%)                        *", i, n, 100.0 * n / _gathererTicks);
      }
    }
    tty->print_cr("*    Early ticks ignored:          %8d    *", _gathererEarlyTicks);
    tty->print_cr("*    GC ticks ignored:             %8d    *", _garbageCollectorTicks);
    tty->print_cr("************************************************");
  }
}

// collect the statistics and test the validity of this tick and then call the method profiler's
// entry function
void MethodCallGatherer::collect_nodes() {
  // Ignore if waiting on the gc
  if (!_is_waiting_on_gc) {
    _is_doing_collection = true;
    _gathererTicks++;
    _gathererTimer.stop();
    int ms = (int)(_gathererTimer.seconds() * 1000.0);
    
    // ensure this was a full collection period and was not shorter than it should have been.
    if (ms < _visibleInterval) {
      _gathererTimer.start();
      _gathererEarlyTicks++;
      
      return;
    }
    
    _gathererTimer.reset();
    _gathererTimer.start();
    
    // Add this tick to the time histogram
    if (ms >= PeriodicTask::max_interval) ms = PeriodicTask::max_interval - 1;
    _gathererTimeHistogram[ms]++;

    // Call entry method for method data collector (actually in java.hpp)
    method_histogram_gatherer();

    is_queued = false;
    
    // note that the final collection period is finished if this value was true
    _task->_waiting_to_finish = false;

    // Make sure we are not racing with the GC.
    assert(!_is_waiting_on_gc, "Full GC dump detected while collecting data.");
    
    _is_doing_collection = false;
  }
}
