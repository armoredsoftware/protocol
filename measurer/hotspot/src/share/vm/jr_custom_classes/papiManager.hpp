#ifndef PAPI_HPP
#define PAPI_HPP

#include "memory/allocation.hpp"
#include "jr_custom_classes/papi_headers/papi.h"

typedef long_long papi_cntr;

class PapiThreadShadow;

// This class provides an interface to the dynamically loaded papi
// library
class PAPI : public AllStatic {
  friend class PapiThreadShadow;

private:
  static void* _library;
  static bool tried_to_load_papi;
  static bool initialized;
  static bool ready;

  static bool enabled;
  static bool multiplex;
  static bool overflow;

  static int* Events;
  static int num_events;
  
  // Types of PAPI library functions
  typedef int (*library_init_fp)(...);
  typedef int (*thread_init_fp)(...);
  typedef int (*multiplex_init_fp)(...);

  typedef int (*num_cmp_hwctrs_fp)(...);

  typedef int (*register_thread_fp)(...);
  typedef int (*unregister_thread_fp)(...);
  typedef int (*set_cmp_granularity_fp)(...);

  typedef int (*assign_eventset_component_fp)(...);
  typedef int (*set_multiplex_fp)(...);
  typedef int (*create_eventset_fp)(...);
  typedef int (*add_event_fp)(...);
  typedef int (*remove_event_fp)(...);
  typedef int (*cleanup_eventset_fp)(...);
  typedef int (*destroy_eventset_fp)(...);

  typedef int (*start_fp)(...);
  typedef int (*stop_fp)(...);
  typedef int (*read_fp)(...);
  typedef int (*reset_fp)(...);
  typedef int (*accum_fp)(...);

  typedef int (*query_event_fp)(...);
  typedef int (*event_code_to_name_fp)(...);
  typedef int (*event_name_to_code_fp)(...);

  typedef int (*shutdown_fp)(...);
  typedef char* (*strerror_fp)(...);


  /* Library interface functions */

  // initialization
  static library_init_fp              library_init;
  static thread_init_fp               thread_init;
  static multiplex_init_fp            multiplex_init;
  
  // hardware info
  static num_cmp_hwctrs_fp            num_cmp_hwctrs;

  // pthread support
  static register_thread_fp           register_thread;
  static unregister_thread_fp         unregister_thread;
  static set_cmp_granularity_fp       set_cmp_granularity;
  
  // eventset management
  static assign_eventset_component_fp assign_eventset_component;
  static set_multiplex_fp             set_multiplex;
  static create_eventset_fp           create_eventset;
  static add_event_fp                 add_event;
  static remove_event_fp              remove_event;
  static cleanup_eventset_fp          cleanup_eventset;
  static destroy_eventset_fp          destroy_eventset;
  
  // counter mangagement
  static start_fp                     start;
  static stop_fp                      stop;
  static read_fp                      read;
  static reset_fp                     reset;
  static accum_fp                     accum;
  
  // event lookup support
  static query_event_fp               query_event;
  static event_code_to_name_fp        event_code_to_name;
  static event_name_to_code_fp        event_name_to_code;
  
  // error handling
  static shutdown_fp                  shutdown;
  static strerror_fp                  strerror;


  // Sets state variables
  static void set_enables();

  // PAPI helper functions
  static int papi_setup_helper();
  static int load_papi();
  static int link_interface();
  static int read_events(int EventSet);
  static int setup_events();
  

public:
  static inline int get_num_events() { return num_events; }

  // Is a mode enabled
  static inline bool is_papi_enabled()      { return enabled; }
  static inline bool is_papi_ready()        { return ready; }
  static inline bool is_multiplex_enabled() { return multiplex; }
  static inline bool is_overflow_enabled()  { return overflow; }

  static inline bool has_papi_failed()      { return !initialized && tried_to_load_papi; }

  static int break_on_bad_errval(int errval);

  static int papi_setup();
  static int papi_tear_down();
};

#endif
