//#include <cstring>
#include "precompiled.hpp"
#include "code/codeCache.hpp"
#include "code/codeBlob.hpp"

#include "jr_custom_classes/fileIO.hpp"
#include "jr_custom_classes/methodGatherer.hpp"
#include "jr_custom_classes/memoryCollector.hpp"

// Initialize static fields
bool OurMemoryCollector::use_memory_map   = false;//JRMethodMemoryProfilerCCMap;
int  OurMemoryCollector::collection_count = 0;

int OurMemoryCollector::mean_heap_used       = 0;
int OurMemoryCollector::max_heap_used        = 0;
int OurMemoryCollector::mean_code_cache_used = 0;
int OurMemoryCollector::max_code_cache_used  = 0;

MemoryNode* OurMemoryCollector::head = NULL;
MemoryNode* OurMemoryCollector::tail = NULL;

void OurMemoryCollector::set_use_memory_map(bool use_map) {
  use_memory_map = use_map;
}

void OurMemoryCollector::cleanup() {
  // delete each MemoryNode in the linked list
  while (head != NULL) {
    MemoryNode* memory_node = head;
    head = head->next;
   
    // delete each map node for the node's map linked list 
    while (memory_node->map_head != NULL) {
      CodeCacheMemoryMapNode* map_node = memory_node->map_head;
      memory_node->map_head = memory_node->map_head->next;
      delete map_node->name;
      delete map_node;
    }
    
    memory_node->map_head = NULL;
    memory_node->map_tail = NULL;
    
    delete memory_node;
  }
  
  // Invalidate dangling pointers
  head = NULL;
  tail = NULL;
}

void OurMemoryCollector::make_map(MemoryNode* holder) {
  CodeBlob* blob = CodeCache::first();
  
  // iterate across the entire code cache creating a node for each blob in it
  while (blob != NULL) {
    make_map_node(blob, holder);
    blob = CodeCache::next(blob);
  }
}

void OurMemoryCollector::make_map_node(CodeBlob* blob, MemoryNode* holder) {
  CodeCacheMemoryMapNode* map_node = new CodeCacheMemoryMapNode();
  
  if (holder->map_head == NULL) {
    // first node addition sets the head and tail
    holder->map_head = map_node;
    holder->map_tail = map_node;
  }
  else {
    // add map_node to the back of the memory map
    holder->map_tail->next = map_node;
    holder->map_tail = map_node;
  }

  // name the map node
  if      (blob->is_buffer_blob())                 map_node->name = new CStringHolder("Buffer Blob");
  else if (blob->is_nmethod()) {
    map_node->name = new CStringHolder("nmethod: ");
    map_node->name->append(((nmethod*) blob)->method_name_holder.get_cstring());
  }
  else if (blob->is_runtime_stub())                map_node->name = new CStringHolder("Runtime Stub");
  else if (blob->is_ricochet_stub())               map_node->name = new CStringHolder("Ricochet Stub");
  else if (blob->is_deoptimization_stub())         map_node->name = new CStringHolder("Deoptimization Stub");
  else if (blob->is_uncommon_trap_stub())          map_node->name = new CStringHolder("Uncommon Trap Stub");
  else if (blob->is_exception_stub())              map_node->name = new CStringHolder("Exception Stub");
  else if (blob->is_safepoint_stub())              map_node->name = new CStringHolder("Safepoint Stub");
  else if (blob->is_adapter_blob())                map_node->name = new CStringHolder("Adapter Blob");
  else if (blob->is_method_handles_adapter_blob()) map_node->name = new CStringHolder("Method Handles Adapter Blob");
  else                                             map_node->name = new CStringHolder("<UNKNOWN BLOB>");

  // Calculate the memory range the blob occupies
  map_node->blob_start = blob->header_begin();
  map_node->blob_end   = blob->header_begin() + blob->size();
} 

// Creates a new MemoryNode, sets all of its values, and adds it to the list
void OurMemoryCollector::make_memory_node() {
  MemoryNode* new_node = new MemoryNode();

  if (head == NULL) {
    head = new_node;
    tail = new_node;
  } 
  else {
    assert(tail != NULL, "Cannot add to list: Tail == NULL");
    
    tail->next = new_node;
    tail = new_node;
  }
  
  // Notes full gc number
  collection_count++;

  new_node->collection_count = collection_count;
  
  // marks the tick at which this gc spans
  new_node->method_gatherer_tick = MethodCallGatherer::_gathererTicks;
  
  // store information about the heap
  new_node->heap_start = (address)Universe::heap()->reserved_region().start();
  new_node->heap_end   = (address)Universe::heap()->reserved_region().end();
  new_node->heap_capacity = Universe::heap()->capacity();
  new_node->heap_used     = Universe::heap()->used();

  // store information about the code cache
  new_node->code_cache_start    = CodeCache::low_bound();
  new_node->code_cache_end      = CodeCache::high_bound();
  new_node->code_cache_capacity = CodeCache::capacity();
  //TODO: Verify this is correct
  new_node->code_cache_used     = CodeCache::_heap->allocated_capacity();
  
  // ensure the pointers are NULL if there is no map
  new_node->map_head = NULL;
  new_node->map_tail = NULL;

  if (use_memory_map) {
    // make the map of the code cache
    MutexLockerEx ml(CodeCache_lock, Mutex::_no_safepoint_check_flag);
    make_map(new_node);
  }
  
  // set the next pointer to NULL
  new_node->next = NULL;
}

void OurMemoryCollector::traverse(void mem_func(MemoryNode*), void map_func(CodeCacheMemoryMapNode*)) {
  if (mem_func == NULL)
    return;

  MemoryNode* m = head;

  while(m != NULL) {
    mem_func(m);
    
    // traverses the map if and only if: 1. We are using a memory map; 2. We have not specified
    //     a NULL function for map_func; 3. The map list is not empty;
    if (use_memory_map && map_func != NULL && m->map_head != NULL) {
      traverse_map(map_func, m);
    }

    m = m->next;
  }
}

// traverse the map of the code cache collected at a memory node
void OurMemoryCollector::traverse_map(void map_func(CodeCacheMemoryMapNode*), MemoryNode* holder) {
  if (holder->map_head == NULL)
    return;
  
  CodeCacheMemoryMapNode* node = holder->map_head;

  while(node != NULL) {
    map_func(node);
    node = node->next;
  }
}

// Prints the MemoryNode information to the console
void OurMemoryCollector::print_memory(MemoryNode* memory_node) {
  tty->print_cr("Collection #%d at tick %d", memory_node->collection_count, memory_node->method_gatherer_tick);
  tty->print_cr("  Heap:");
  tty->print_cr("    Spans: " INTPTR_FORMAT " - " INTPTR_FORMAT, memory_node->heap_start, memory_node->heap_end);
  tty->print_cr("    Capacity: %d B", memory_node->heap_capacity);
  tty->print_cr("    Used:     %d B (%d%%)", memory_node->heap_used, ((memory_node->heap_used * 100) / memory_node->heap_capacity));
  tty->print_cr("  Code Cache:");
  tty->print_cr("    Spans: " INTPTR_FORMAT " - " INTPTR_FORMAT, memory_node->code_cache_start, memory_node->code_cache_end);
  tty->print_cr("    Capacity: %d B", memory_node->code_cache_capacity);
  tty->print_cr("    Used:     %d B (%d%%)", memory_node->code_cache_used, ((memory_node->code_cache_used * 100) / memory_node->code_cache_capacity));
}

// Prints the map node's information to the console
void OurMemoryCollector::print_map(CodeCacheMemoryMapNode* map_node) {
  tty->print_cr("      " INTPTR_FORMAT " - " INTPTR_FORMAT " | %s", map_node->blob_start, map_node->blob_end, map_node->name->get_cstring());
}

// Prints the memory node's information to the console
void OurMemoryCollector::output_memory(MemoryNode* memory_node) {
  OurFileStream* OFS = OurFileStream::memory_fstream;

  OFS->write_cstring("GC_Collection ");
  OFS->write_data(memory_node->collection_count);
  OFS->new_line();

  // Output heap statistics
  OFS->write_cstring("Heap_spans: ");
  OFS->write_data((int)memory_node->heap_start, "%x");
  OFS->write_cstring("-");
  OFS->write_data((int)memory_node->heap_end, "%x");
  OFS->new_line();
  OFS->write_cstring("Heap_capacity: ");
  OFS->write_data(memory_node->heap_capacity);
  OFS->new_line();
  OFS->write_cstring("Heap_usage: ");
  OFS->write_data(memory_node->heap_used);
  OFS->new_line();
  // Output code cache satistics
  OFS->write_cstring("Code_cache_spans: ");
  OFS->write_data((int)memory_node->code_cache_start, "%x");
  OFS->write_cstring("-");
  OFS->write_data((int)memory_node->code_cache_end, "%x");
  OFS->new_line();
  OFS->write_cstring("Code_cache_capacity: ");
  OFS->write_data(memory_node->code_cache_capacity);
  OFS->new_line();
  OFS->write_cstring("Code_cache_used: ");
  OFS->write_data(memory_node->code_cache_used);
  OFS->new_line();
  // Indication of whether a map follows
  OFS->write_cstring("Map: ");
  OFS->write_data(memory_node->map_head != NULL);
  OFS->new_line();
  // Write the code cache map if there is one
  if (memory_node->map_head != NULL) {
    traverse_map(output_map, memory_node);
  }
  // Make a blank line to separate MemoryNode outputs
  OFS->new_line();
}

// Output the map to a file
void OurMemoryCollector::output_map(CodeCacheMemoryMapNode* map_node) {
  OurFileStream* OFS = OurFileStream::memory_fstream;

  // Write the span of the code blob
  OFS->write_data((int)map_node->blob_start, "%x");
  OFS->write_cstring("-");
  OFS->write_data((int)map_node->blob_end, "%x");
  OFS->write_cstring(" \"");
  OFS->write_cstring(map_node->name->get_cstring());
  OFS->write_cstring("\"");
  OFS->new_line();
}

// collect the memory data
void OurMemoryCollector::do_collect() {
  make_memory_node();
}

// evaluate our statistics
void OurMemoryCollector::do_statistics() {
  MemoryNode* node = head;

  // Error case for no full GCs reached
  if (collection_count <= 0) {
    mean_heap_used = -1;
    mean_code_cache_used = -1;
    max_heap_used = -1;
    max_code_cache_used = -1;
    
    return;
  }

  // Ensure all fields are set to 0
  mean_heap_used = 0;
  mean_code_cache_used = 0;
  max_heap_used = 0;
  max_code_cache_used = 0;
  
  // collect the max and total values of all code cache and heap sizes 
  while (node != NULL) {
    mean_heap_used       += (int) node->heap_used;
    mean_code_cache_used += (int) node->code_cache_used;
    
    if ((int) node->heap_used > max_heap_used) {
      max_heap_used = (int) node->heap_used;
    }
    if ((int) node->code_cache_used > max_code_cache_used) {
      max_code_cache_used = (int) node->code_cache_used;
    }

    node = node->next;
  }

  // find the mean values of the code cache and heap sizes
  mean_heap_used /= collection_count;
  mean_code_cache_used /= collection_count;
}

// Prints the collector to the console along with the statistics
void OurMemoryCollector::print_collector() {
  tty->print_cr("************* MemoryData **************");
  traverse(&print_memory, &print_map);
  tty->print_cr("Overview:");
  tty->print_cr("  Heap:");
  tty->print_cr("    Mean Usage: %10d B", mean_heap_used);
  tty->print_cr("    Max Usage:  %10d B", max_heap_used);
  tty->print_cr("  Code Cache:");
  tty->print_cr("    Mean Usage: %10d B", mean_code_cache_used);
  tty->print_cr("    Max Usage:  %10d B", max_code_cache_used);
}

// Writes the collector to a file
void OurMemoryCollector::output_collector() {
  OurFileStream* OFS = OurFileStream::memory_fstream = OurFileStream::initialize_our_output_stream("Memory");
  
  // Output generic heading
  OFS->write_cstring("@ + heading");
  OFS->new_line();
  OFS->write_heading();
  OFS->new_line();
  OFS->write_cstring("@ - heading");
  OFS->new_line();
  OFS->new_line();

  // Output data
  OFS->write_cstring("@ + data memory");

  // traverse the MemoryNode list and output each node's data
  traverse(output_memory, NULL);

  // write overall statistics
  OFS->write_cstring("Overview:");
  OFS->new_line();
  OFS->write_cstring("Heap_max_usage:        ");
  OFS->write_data(max_heap_used, "%10d");
  OFS->new_line();
  OFS->write_cstring("Heap_mean_usage:       ");
  OFS->write_data(mean_heap_used, "%10d");
  OFS->new_line();
  OFS->write_cstring("Code_cache_max_usage:  ");
  OFS->write_data(max_code_cache_used, "%10d");
  OFS->new_line();
  OFS->write_cstring("Code_cache_mean_usage: ");
  OFS->write_data(mean_code_cache_used, "%10d");
  OFS->new_line();

  OFS->write_cstring("@ - data");
  OFS->new_line();
  OFS->write_cstring("@ + EOF");
  OFS->end_our_output_stream();
}

MethodMemoryNode* MethodMemoryProfiler::head = NULL;
MethodMemoryNode* MethodMemoryProfiler::tail = NULL;


// Add a method to the memory profiler's list
void MethodMemoryProfiler::add_nmethod(nmethod* nm)
{
  ResourceMark rm;
  // Create the node
  MethodMemoryNode* node = new MethodMemoryNode();
  node->name = new CStringHolder(nm->method()->name_and_sig_as_C_string());
  node->nmethod_size = nm->total_size() + sizeof(*nm);

  // Add the node to the list
  if (head == NULL) {
    head = node;
    tail = node;
  }
  else {
    tail->next = node;
    tail = node;
  }
}

void MethodMemoryProfiler::destroy_list()
{
  tail = NULL;

  while (head != NULL) {
    MethodMemoryNode* temp = head;
  
    head = head->next;
  
    delete temp;
  }
}

void MethodMemoryProfiler::print_nmethods()
{
  MethodMemoryNode* node = head;

  while (node != NULL) {
    tty->print_cr("NMS %s %d", node->name->get_cstring(), node->nmethod_size);
    node = node->next;
  }
}

void MethodMemoryProfiler::output_nmethods()
{
  /*MethodMemoryNode* node = head;

  while (node != NULL) {
    MethodMemoryNode* temp = head;
    while (node != temp && temp != NULL) {
      if (strcmp(node->name->get_cstring(), temp->name->get_cstring())) { 
	temp = NULL;
      }
    }
    
    if (temp != NULL)
      tty->print_cr("%s %d", node->name->get_cstring(), node->nmethod_size);
      }*/
}
