
#ifndef METHOD_CHECK_IN_HPP
#define METHOD_CHECK_IN_HPP

#include "memory/allocation.hpp"
#include "code/nmethod.hpp"

class MethodCheckInTask;

// This class controls the periodic task resposible for
// checking the value we write to on each invocation of
// a compiled nmethod. We then use the value to determine
// what our course of action should be with reguards to
// that method
class MethodCheckInHandler: AllStatic
{
  friend class MethodCheckInTask;
 private:
  static MethodCheckInTask* _task;
  
 public:  
  // Define what each bit in the our_compile_run_status variable means
  enum CheckInBits {
    not_compiled =              0b1000,   // 1 if method is not compiled
    compile_not_used =          0b0100,   // 1 if method has never been
    // invoked while compiled
    compile_seen_recent =       0b0010,   // 1 if method has been invoked
    // since the last time we scanned
    compile_active =            0b0001    // 1 if method has checked in
    // but not subsequently back out.
  };

  // Initialize and destroy the periodic task.
  static void engage();
  static void disengage();

  static jlong current_tick();
};

class MethodCheckInStatus// : public ResourceObj
{
 private:
  unsigned char value;

 public:
  MethodCheckInStatus(unsigned char init_value) { value = init_value; }
  
  address get_address() { return &value; }
  void set_value(unsigned char new_value) { value = new_value; }
  unsigned char get_value() { return value; }

};

#endif
  
