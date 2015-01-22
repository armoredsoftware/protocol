#ifndef _METHOD_CALL_GATHERER
#define _METHOD_CALL_GATHERER

#include "runtime/task.hpp"
#include "runtime/timer.hpp"
#include "memory/allocation.hpp"

class MethodCallGathererTask;

// The manager class for our periodic task. This does most of the work of it's the periodic task.
class MethodCallGatherer : AllStatic
{
  friend class VM_Our_CallMethodCollector;
  friend class MethodCallGathererTask;
private:
  // The periodic task
  static MethodCallGathererTask* _task;

  // Indicates whether or not the vm_operation has been queued so we know not to queue it again
  static bool is_queued;
  
public:
  // The current tick being collected
  static int _gathererTicks;

  // The ticks we are ignoring due to early triggers
  static int _gathererEarlyTicks;

  // The ticks skipped to ensure safety across a garbage collection
  static int _garbageCollectorTicks;

  // Collects the actual time elapsed between ticks to help consistency (reads from _gathererTimer)
  static int _gathererTimeHistogram[PeriodicTask::max_interval];
  
  // The ideal interval between ticks (a copy of the private value in PeriodicTask)
  static int _visibleInterval;

  // Indicates whether we are already waiting on gc so we do not queue the vm opperation more than
  // once.
  static volatile bool _is_waiting_on_gc;
  
  // Indicates to the GC that we are collecting data and should wait until we are finished
  static volatile bool _is_doing_collection;

  // The actual time measured time since the last collection interval
  static elapsedTimer _gathererTimer;
  
  // Functions to register and un-register the PeriodicTask with the WatcherThread
  static void engage();
  static void disengage();

  // Functions to call the data collector
  static void collect_nodes();
  static void collect_final_interval();

  // Console and File IO calls
  static void print_gatherer_intervals();
  static void print_statistics_and_output_data(); 
};

// The periodic task that profiles calls the data collector in our profile run. This class is simply a
// timer that, when triggered, calls functions from its mangager task as well as some vm_opperations.
class MethodCallGathererTask : public PeriodicTask
{
  friend class VM_Our_CallMethodCollector;
  friend class MethodCallGatherer;

private:
  volatile bool _waiting_to_finish;
  volatile bool _running;
public:
  MethodCallGathererTask(int interval_time) : PeriodicTask(interval_time), _waiting_to_finish(false), _running(false)
    { MethodCallGatherer::_visibleInterval = interval_time; }

  // The task to execute each time interval
  void task();
};

#endif
