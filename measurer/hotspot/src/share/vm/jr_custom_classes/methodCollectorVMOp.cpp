#include "precompiled.hpp"
#include "runtime/task.hpp"
#include "runtime/vm_operations.hpp"

#include "jr_custom_classes/methodGatherer.hpp"

// Basic Constructor
VM_Our_CallMethodCollector::VM_Our_CallMethodCollector() {};

// Ensure execution occurs at a safepoint (after GC is complete)
VM_Operation::Mode VM_Our_CallMethodCollector::evaluation_mode() const            { return VM_Operation::_safepoint; }

// Main vm opperation instructions
void VM_Our_CallMethodCollector::doit() {
  MethodCallGatherer::collect_nodes();
}
