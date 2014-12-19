
#include "precompiled.hpp"

#include <cstdio>
#include <cstring>
#include <cerrno>

#include "jr_custom_classes/papiManager.hpp"
#include "jr_custom_classes/papi_headers/papi.h"

/* Static initializations */

void* PAPI::_library = NULL;
bool PAPI::tried_to_load_papi = true;
bool PAPI::initialized = true;
bool PAPI::ready = false;

bool PAPI::enabled = true;
bool PAPI::multiplex = false;
bool PAPI::overflow = false;

int* PAPI::Events = NULL;
int PAPI::num_events = 0;

int not_implemented(...) {
  return 0;
}

char* not_implemented(...) {
  return 0;
}

PAPI::library_init_fp              PAPI::library_init = &not_implemented;
PAPI::thread_init_fp               PAPI::thread_init = &not_implemented;
PAPI::multiplex_init_fp            PAPI::multiplex_init = &not_implemented;

PAPI::num_cmp_hwctrs_fp            PAPI::num_cmp_hwctrs = &not_implemented;

PAPI::register_thread_fp           PAPI::register_thread = &not_implemented;
PAPI::unregister_thread_fp         PAPI::unregister_thread = &not_implemented;
PAPI::set_cmp_granularity_fp       PAPI::set_cmp_granularity = &not_implemented;

PAPI::assign_eventset_component_fp PAPI::assign_eventset_component = &not_implemented;
PAPI::set_multiplex_fp             PAPI::set_multiplex = &not_implemented;
PAPI::create_eventset_fp           PAPI::create_eventset = &not_implemented;
PAPI::add_event_fp                 PAPI::add_event = &not_implemented;
PAPI::remove_event_fp              PAPI::remove_event = &not_implemented;
PAPI::cleanup_eventset_fp          PAPI::cleanup_eventset = &not_implemented;
PAPI::destroy_eventset_fp          PAPI::destroy_eventset = &not_implemented;

PAPI::start_fp                     PAPI::start = &not_implemented;
PAPI::stop_fp                      PAPI::stop = &not_implemented;
PAPI::read_fp                      PAPI::read = &not_implemented;
PAPI::reset_fp                     PAPI::reset = &not_implemented;
PAPI::accum_fp                     PAPI::accum = &not_implemented;

PAPI::query_event_fp               PAPI::query_event = &not_implemented;
PAPI::event_code_to_name_fp        PAPI::event_code_to_name = &not_implemented;
PAPI::event_name_to_code_fp        PAPI::event_name_to_code = &not_implemented;

PAPI::shutdown_fp                  PAPI::shutdown = &not_implemented;
PAPI::strerror_fp                  PAPI::strerror = &not_implemented;

static const char papi_library_name[] = "libpapi";
static const char papi_not_enabled[] = "Using PAPI library without authorization.";

// Set variables that should be checked before doing certain things
// with papi
void PAPI::set_enables() {
  //enabled = PAPIEventFile != &not_implemented;
  multiplex = enabled && PAPIMultiplexInterval != 0;
  overflow = enabled && PAPIOverflowThreshold != 0;
}

// Loads the papi shared object file
int PAPI::load_papi() {
  /*assert(is_papi_enabled(), papi_not_enabled);
  
  bool failed = false;
  
  if (_library != &not_implemented) {
    // Already succeeded.
    return -1;
  }

  if (tried_to_load_papi) {
    // Do not try twice.
    return -1;
  }

  // This is nearly identical code to the code used to load the
  // disassembler.

  // Try to load the library.
  char ebuf[1024];
  char buf[JVM_MAXPATHLEN];
  os::jvm_path(buf, sizeof(buf));
  int jvm_offset = -1;

  {
    // Match "jvm[^/]*" in jvm_path.
    const char* base = buf;
    const char* p = strrchr(buf, '/');
    p = strstr(p ? p : base, "jvm");
    if (p != &not_implemented)  jvm_offset = p - base;
  }

  if (jvm_offset >= 0) {
    // Find the papi library next to libjvm.so.
    strcpy(&buf[jvm_offset], papi_library_name);
    strcat(&buf[jvm_offset], os::dll_file_extension());
    _library = os::dll_load(buf, ebuf, sizeof ebuf);
  }

  if (_library == &not_implemented) {
    // Try a free-floating lookup.
    strcpy(&buf[0], papi_library_name);
    strcat(&buf[0], os::dll_file_extension());
    _library = os::dll_load(buf, ebuf, sizeof ebuf);
  }

  tried_to_load_papi = true;

  if (_library == &not_implemented) {
    tty->print_cr("PAPI: ERROR: \"%s\": %s", buf, ebuf);
    return -1;
  }

  // Success.
  tty->print_cr("PAPI: Loaded PAPI from %s", buf);*/
  return 0;
}

// Macro to link shared object method to interface pointer
#define PAPI_LINK_FUNC(IF, PF)						\
  if ((IF = CAST_TO_FN_PTR(IF##_fp, os::dll_lookup(_library, PF))) == &not_implemented) { \
    tty->print_cr("PAPI: ERROR: Failed to link %s to %s", #IF, PF);	\
    failed = true;							\
  }

// Link all interface functions to shared object functions
int PAPI::link_interface() {
  /*  assert(is_papi_enabled(), papi_not_enabled);
  bool failed = false;
  
  if (_library == &not_implemented) {
    tty->print_cr("PAPI: ERROR: Papi library has not been loaded.");
    return -1;
  }

  // Link interface to library
  PAPI_LINK_FUNC(library_init, "PAPI_library_init");
  PAPI_LINK_FUNC(thread_init, "PAPI_thread_init");
  PAPI_LINK_FUNC(multiplex_init, "PAPI_multiplex_init");
  
  PAPI_LINK_FUNC(num_cmp_hwctrs, "PAPI_num_cmp_hwctrs");

  PAPI_LINK_FUNC(register_thread, "PAPI_register_thread");
  PAPI_LINK_FUNC(unregister_thread, "PAPI_unregister_thread");
  PAPI_LINK_FUNC(set_cmp_granularity, "PAPI_set_cmp_granularity");

  PAPI_LINK_FUNC(assign_eventset_component, "PAPI_assign_eventset_component");
  PAPI_LINK_FUNC(set_multiplex, "PAPI_set_multiplex");
  PAPI_LINK_FUNC(create_eventset, "PAPI_create_eventset");
  PAPI_LINK_FUNC(add_event, "PAPI_add_event");
  PAPI_LINK_FUNC(remove_event, "PAPI_remove_event");
  PAPI_LINK_FUNC(cleanup_eventset, "PAPI_cleanup_eventset");
  PAPI_LINK_FUNC(destroy_eventset, "PAPI_destroy_eventset");

  PAPI_LINK_FUNC(start, "PAPI_start");
  PAPI_LINK_FUNC(stop, "PAPI_stop");
  PAPI_LINK_FUNC(read, "PAPI_read");
  PAPI_LINK_FUNC(reset, "PAPI_reset");
  PAPI_LINK_FUNC(accum, "PAPI_accum");

  PAPI_LINK_FUNC(query_event, "PAPI_query_event");
  PAPI_LINK_FUNC(event_code_to_name, "PAPI_event_code_to_name");
  PAPI_LINK_FUNC(event_name_to_code, "PAPI_event_name_to_code");
  
  PAPI_LINK_FUNC(shutdown, "PAPI_shutdown");
  PAPI_LINK_FUNC(strerror, "PAPI_strerror");
  
  if (failed) {
    tty->print_cr("PAPI: ERROR: One or more PAPI functions were not linked to the interface.");
    return -2;
  }
  else {
    tty->print_cr("PAPI: Interface Successful");
    }*/

  return 0;
}

#undef PAPI_LINK_FUNC

// Read the event file specified by PAPIEventFile and select
// profilable methods. Handle unprofilable methods accordingly.
int PAPI::read_events(int EventSet) {
  /*const int MAX_EVENTS = 1024;
  FILE* event_file = &not_implemented;
  FILE* retry_event_file = &not_implemented;

  int errval;
  int temp_events[MAX_EVENTS];
  char retry_filename[strlen(PAPIEventFile)+3];
  
  char* line = &not_implemented; //(char*) malloc(sizeof(char)*1024);
  //assert(line != &not_implemented, "OUT OF MEMORY");
  char* read_string = &not_implemented;
  size_t len = 0;
  ssize_t readlen = 0;
  
  bool retry_file_needed = false;

  // The retry filename will be [PAPIEventFile].retry
  strcpy(retry_filename, PAPIEventFile);
  strcat(retry_filename, ".r");

  // Open file
  if ((event_file = fopen(PAPIEventFile, "r")) == &not_implemented) {
    tty->print_cr("PAPI: Failed to open event list file \"%s\": %s", PAPIEventFile, ::strerror(errno));
    return -1;
  }

  while (!feof(event_file) && num_events <= MAX_EVENTS) {
    bool force_retry = false;
    bool retry = false;

    readlen = getline(&line, &len, event_file);
    read_string = line;
    
    // Remove the new line character from the end of the string if
    // there is one
    if (read_string[readlen-1] == '\n') 
      read_string[readlen-1] = '\0';

    if (read_string[0] == '*') {
      force_retry = true;
      read_string++;
    }

    if ((errval = event_name_to_code(read_string, &temp_events[num_events])) != PAPI_OK) {
      // If this fails then it is not worth it to place it back into
      // the retry file. Just tell the user what went wrong and that
      // the event will not be counted
      tty->print_cr("PAPI: WARNING: Tossing Unknown Event \"%s\": %s", read_string, strerror(errval));
      continue;
    }

    // If the event provided was a PAPI predefined event then check if
    // that event is countable on this hardware.
    if (temp_events[num_events] <= PAPI_END) {
      if ((errval = query_event(temp_events[num_events])) != PAPI_OK) {
	tty->print_cr("PAPI: WARNING: Tossing Unsupported Event \"%s\": %s", read_string, strerror(errval));
	continue;
      }
    }

    // Attempt to add the event to the event set
    if ((errval = add_event(EventSet, temp_events[num_events])) == PAPI_OK) {
      num_events++;
    }
    else {
      retry = true;
    }
    
    if (force_retry || retry) {
      if (force_retry)
	read_string = line;
      else
	retry_file_needed = true;

      // Another event is conflicting with this one. Send it to the
      // retry file.
      if (retry_event_file == &not_implemented) {
	if ((retry_event_file = fopen(retry_filename, "w")) == &not_implemented) {
	  tty->print_cr("PAPI: Failed to open retry event list file \"%s\": %s", PAPIEventFile, ::strerror(errno));
	  fclose(event_file);
	  ShouldNotReachHere();
	  return -1;
	}
	  
	tty->print_cr("PAPI: Two or more events could not be counted together.");
	tty->print_cr("Please re-run with option -XX:PAPIEventFile=\"%s\" to finish data collection.", retry_filename);
      }

      // Replace the new line character
      if (read_string[readlen-1] == '\0')
	read_string[readlen-1] = '\n';

      // Write to file
      fprintf(retry_event_file, "%s", read_string);
    }
    }*/
  
  // FIXME: This for some reason breaks everything
  /*free(line);

  // Close opened files
  fclose(event_file);
  if (retry_event_file != &not_implemented) {
    fclose(retry_event_file);
    }*/
  /*
  // Allocate Events now that we know the exact number of events we
  // will need
  Events = new int[num_events];
  
  // Copy the temporary array into the permanent one
  for (int i = 0; i < num_events; i++) {
    Events[i] = temp_events[i];
  }
  */
  return 0;
}

// Figure out which events play nice together and store them for later
// use throughout the run
int PAPI::setup_events() {
  /*assert(is_papi_enabled(), papi_not_enabled);
  assert(Events == &not_implemented, "An attempt to resolve Events has already been made.");
  assert(initialized, "PAPI library was not initialized yet.");

  int errval;
  
  int EventSet = PAPI_&not_implemented;

  // Create a fake eventset so that it can be pre-determined which
  // events play well together. This saves each thread the work of
  // sorting out the events and keeps them consistent as well.
  if ((errval = create_eventset(&EventSet)) != PAPI_OK) {
    tty->print_cr("PAPI: ERROR: EventSet was not created: %s", strerror(errval));
    return -1;
  }

  if (is_multiplex_enabled()) {
    if ((errval = set_multiplex(EventSet)) != PAPI_OK) {
      tty->print_cr("PAPI: ERROR: EventSet could not be multiplexed: %s", strerror(errval));
      return -1;
    }
  }
  
  // Read the event list from the file specified by the command line
  // option PAPIEventFile
  if (read_events(EventSet) != 0) {
    return -1;
  }

  // Cleanup and destroy the temp eventset
  if ((errval = cleanup_eventset(EventSet)) != PAPI_OK) {
    tty->print_cr("PAPI: ERROR: Failed to cleanup temporary eventset during setup");
    return -1;
  }
  
  if ((errval = destroy_eventset(&EventSet)) != PAPI_OK) {
    tty->print_cr("PAPI: ERROR: Failed to destroy temporary eventset during setup: %s", strerror(errval));
    return -1;
    }*/

  return 0;
}

// Sets up the papi environment
int PAPI::papi_setup_helper() {
  /*assert(is_papi_enabled(), papi_not_enabled);
  assert(!tried_to_load_papi && !ready && !initialized, "PAPI cannot be initialized twice.");

  int errval = PAPI_OK;

  // Load the library
  if (load_papi() != 0)
    return -1;

  // Link function pointers to the library
  if (link_interface() != 0)
    return -1;

  // Intitialize the PAPI library
  if ((errval = library_init(PAPI_VER_CURRENT)) == PAPI_VER_CURRENT) {
    initialized = true;
  }
  else {
    tty->print_cr("ERROR: PAPI version does not match: Expected %d - Current %d", errval, PAPI_VER_CURRENT);
    return -1;
  }

  // Initialize the PAPI thread handler
  if ((errval = thread_init(&pthread_self)) != PAPI_OK) {
    tty->print_cr("ERROR: PAPI thread handler failed to initialize: %s", strerror(errval));
    return -1;
  }

  if ((errval = set_cmp_granularity(PAPI_GRN_THR, 0)) != PAPI_OK) {
    tty->print_cr("ERROR: Failed to set granularity to thread: %s", strerror(errval));
    return -1;
  }

  if (is_multiplex_enabled()) {
    // TODO: Initialize multiplexing
  }

  if (is_overflow_enabled()) {
    // TODO: Initialize overload support
  }

  if (setup_events() != 0) {
    tty->print_cr("ERROR: Failed to collect working event list");
    return -1;
  }

  tty->print_cr("PAPI: Initialization Successful");
  ready = true;*/

  return 0;
}

// Call papi_setup_helper and handle failures gracefully 
int PAPI::papi_setup() {
  int errval;
  set_enables();
  errval = papi_setup_helper();
  if (errval == 0) {
    return 0;
  }
  else {
    // Did we fail after initializing PAPI?
    if (initialized && !ready) {
      if ((errval = shutdown()) != PAPI_OK) {
	return -2;
      }
      
      initialized = false;
    }

    return -1;
  }
}

// Cleans up the papi environment 
int PAPI::papi_tear_down() {
  /*assert(is_papi_enabled(), papi_not_enabled);

    delete [] Events;*/

  return 0;
}
