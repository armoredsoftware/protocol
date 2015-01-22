#ifndef _JR_MEMORY_COLLECTOR
#define _JR_MEMORY_COLLECTOR

#include "memory/allocation.hpp"
#include "code/codeBlob.hpp"

#include "jr_custom_classes/fileIO.hpp"

// Memory map linked list node
typedef struct CodeCacheMemoryMapNode {
  // Name of CodeBlob
  CStringHolder* name;
  // Bounds of the CodeBlob
  address blob_start;
  address blob_end;
  
  CodeCacheMemoryMapNode* next;
} CodeCacheMemoryMapNode;

// Primary memory linked list node
typedef struct MemoryNode {
  // GC Count
  int collection_count;

  // Indicates what tick this data was collected at to align it with the
  // method collection process
  // set with methodGatherer::_gathererTicks
  int method_gatherer_tick;

  // Heap data
  address heap_start;
  address heap_end;
  size_t heap_capacity;
  size_t heap_used;

  // Code Cache data
  address code_cache_start;
  address code_cache_end;
  size_t code_cache_capacity;
  size_t code_cache_used;
  
  // Pointers to the head and tail of the code cache map
  CodeCacheMemoryMapNode* map_head;
  CodeCacheMemoryMapNode* map_tail;

  // Pointer to the next node in the memory map
  MemoryNode* next;
} MemoryNode;

class OurMemoryCollector : AllStatic {
private:
  // Indicates whether a memory map should be used
  static bool use_memory_map;

  // Memory list size
  static int collection_count;

  // Set by do_statistics
  static int mean_heap_used;
  static int max_heap_used;
  static int mean_code_cache_used;
  static int max_code_cache_used;

  // Pointers to the head and tail of the linked list
  static MemoryNode* head;
  static MemoryNode* tail;

protected:
  // Create a map of the code cache
  static void make_map(MemoryNode* holder);

  // Make new nodes
  static void make_memory_node();
  static void make_map_node(CodeBlob* blob, MemoryNode* holder);
  
  // Traversal functions
  static void traverse(void mem_func(MemoryNode*), void map_func(CodeCacheMemoryMapNode*));
  static void traverse_map(void map_func(CodeCacheMemoryMapNode*), MemoryNode* holder);
  
  // For use with traverse and traverse_map
  // Prints linked list data to the console
  static void print_memory(MemoryNode* memory_node);
  static void print_map(CodeCacheMemoryMapNode* map_node);

  // For use with traverse and traverse_map
  // Outputs linked list data to a file
  static void output_memory(MemoryNode* memory_node);
  static void output_map(CodeCacheMemoryMapNode* map_node);

public:  
  //OurMemoryCollector(bool use_map);
  //~OurMemoryCollector();

  // Set's whether a memory map should be created
  static void set_use_memory_map(bool use_map);

  // Deletes each node and all map nodes belonging to each node in the linked list
  static void cleanup();

  // Getter functions
  static bool is_using_map() { return use_memory_map; }
  static int get_gc_count() { return collection_count; }

  // Does data collection
  static void do_collect();
  
  // Calculates statistics about the memory during the run as a whole
  static void do_statistics();

  // File output and print to console functions
  static void print_collector();
  static void output_collector();
};

typedef struct MethodMemoryNode
{
  CStringHolder* name;

  int nmethod_size;
  MethodMemoryNode* next;
} MethodMemoryNode;

// Collects the memory used by a method. Gathered when method
// compilation is finished.
class MethodMemoryProfiler : public AllStatic
{
private:
  static MethodMemoryNode* head;
  static MethodMemoryNode* tail;
public:
  static void add_nmethod(nmethod* nm);
  static void destroy_list();

  static void print_nmethods();
  static void output_nmethods();
};

#endif
