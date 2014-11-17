#include "runtime/task.hpp"
#include "runtime/timer.hpp"
#include "memory/allocation.hpp"

#include <string>

class TalkThread: public Thread {
  friend class VMStructs;
 public:
  virtual void run();

 private:
  static TalkThread* _talk_thread;

  volatile static bool _should_terminate; // updated without holding lock
 public:
  enum SomeConstants {
    delay_interval = 10                          // interrupt delay in milliseconds
    };

  // Constructor
  TalkThread();

  // Tester
  bool is_Talk_thread() const                 { return true; }

  // Printing
  char* name() const { return (char*)"VM Talk Thread"; }
  void print_on(outputStream* st) const;
  void print() const { print_on(tty); }

  static std::string ExecuteOperation(int local);

  // Returns the single instance of TalkThread
  static TalkThread* talk_thread()         { return _talk_thread; }

  // Create and start the single instance of TalkThread, or stop it on shutdown
  static void start();
  static void stop();
  };
