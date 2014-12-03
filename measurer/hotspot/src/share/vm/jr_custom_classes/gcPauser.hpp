#ifndef _GC_PAUSER_HPP_
#define _GC_PAUSER_HPP_

class GCPauser : public AllStatic 
{
private:
  static bool gc_running;
  static bool pauser_running;

public:
  static bool get_gc_running()               { return gc_running; }
  static bool get_pauser_running()           { return pauser_running; }
  static void set_gc_running(bool state)     { gc_running = state; }
  static void set_pauser_running(bool state) { pauser_running = state; }
};

#endif
