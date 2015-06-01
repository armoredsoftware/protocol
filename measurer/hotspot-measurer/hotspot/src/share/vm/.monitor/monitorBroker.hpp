//#ifndef SHARE_VM_COMPILER_COMPILEBROKER_HPP
//#define SHARE_VM_COMPILER_COMPILEBROKER_HPP

// MonitorTask
//
// An entry in the monitor queue.  It represents a pending or current
// monitor task.
class MonitorTask : public CHeapObj {
 private:
  Monitor*     _lock;
  uint         _compile_id;
  jobject      _method;
  int          _osr_bci;
  bool         _is_complete;
  bool         _is_success;
  bool         _is_blocking;
  int          _comp_level;
  int          _num_inlined_bytecodes;
  nmethodLocker* _code_handle;  // holder of eventual result
  CompileTask* _next, *_prev;

  // Fields used for logging why the compilation was initiated:
  jlong        _time_queued;  // in units of os::elapsed_counter()
  jobject      _hot_method;   // which method actually triggered this task
  int          _hot_count;    // information about its invocation counter
  const char*  _comment;      // more info about the task

 public:
  CompileTask() {
    _lock = new Monitor(Mutex::nonleaf+2, "CompileTaskLock");
  }

  void initialize(int compile_id, methodHandle method, int osr_bci, int comp_level,
                  methodHandle hot_method, int hot_count, const char* comment,
                  bool is_blocking);

  void free();

  int          compile_id() const                { return _compile_id; }
  jobject      method_handle() const             { return _method; }
  int          osr_bci() const                   { return _osr_bci; }
  bool         is_complete() const               { return _is_complete; }
  bool         is_blocking() const               { return _is_blocking; }
  bool         is_success() const                { return _is_success; }

  nmethodLocker* code_handle() const             { return _code_handle; }
  void         set_code_handle(nmethodLocker* l) { _code_handle = l; }
  nmethod*     code() const;                     // _code_handle->code()
  void         set_code(nmethod* nm);            // _code_handle->set_code(nm)

  Monitor*     lock() const                      { return _lock; }

  void         mark_complete()                   { _is_complete = true; }
  void         mark_success()                    { _is_success = true; }

  int          comp_level()                      { return _comp_level;}
  void         set_comp_level(int comp_level)    { _comp_level = comp_level;}

  int          num_inlined_bytecodes() const     { return _num_inlined_bytecodes; }
  void         set_num_inlined_bytecodes(int n)  { _num_inlined_bytecodes = n; }

  CompileTask* next() const                      { return _next; }
  void         set_next(CompileTask* next)       { _next = next; }
  CompileTask* prev() const                      { return _prev; }
  void         set_prev(CompileTask* prev)       { _prev = prev; }

private:
  static void  print_compilation_impl(outputStream* st, methodOop method, int compile_id, int comp_level, bool is_osr_method = false, int osr_bci = -1, bool is_blocking = false, const char* msg = NULL);

public:
  void         print_compilation(outputStream* st = tty);
  static void  print_compilation(outputStream* st, const nmethod* nm, const char* msg = NULL) {
    print_compilation_impl(st, nm->method(), nm->compile_id(), nm->comp_level(), nm->is_osr_method(), nm->is_osr_method() ? nm->osr_entry_bci() : -1, /*is_blocking*/ false, msg);
  }

  static void  print_inlining(outputStream* st, ciMethod* method, int inline_level, int bci, const char* msg = NULL);
  static void  print_inlining(ciMethod* method, int inline_level, int bci, const char* msg = NULL) {
    print_inlining(tty, method, inline_level, bci, msg);
  }

  static void  print_inline_indent(int inline_level, outputStream* st = tty);

  void         print();
  void         print_line();
  void         print_line_on_error(outputStream* st, char* buf, int buflen);

  void         log_task(xmlStream* log);
  void         log_task_queued();
  void         log_task_start(CompileLog* log);
  void         log_task_done(CompileLog* log);
};

// MonitorQueue
//
// A list of MonitorTasks.
class MonitorQueue : public CHeapObj {
 private:
  const char* _name;
  Monitor*    _lock;

  CompileTask* _first;
  CompileTask* _last;

  int _size;
 public:
  MonitorQueue(const char* name, Monitor* lock) {
    _name = name;
    _lock = lock;
    _first = NULL;
    _last = NULL;
    _size = 0;
  }

  const char*  name() const                      { return _name; }
  Monitor*     lock() const                      { return _lock; }

  void         add(CompileTask* task);
  void         remove(CompileTask* task);
  MonitorQueue* first()                           { return _first; }
  MonitorQueue* last()                            { return _last;  }

  MonitorQueue* get();

  bool         is_empty() const                  { return _first == NULL; }
  int          size()     const                  { return _size;          }

  void         print();
};

//#endif // SHARE_VM_COMPILER_COMPILEBROKER_HPP
