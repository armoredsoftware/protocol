#ifndef _METHOD_DATA_COLLECTOR
#define _METHOD_DATA_COLLECTOR

#include "runtime/timer.hpp"
#include "oops/methodOop.hpp"
#include "jr_custom_classes/fileIO.hpp"

// The structure that stores the tick number the node was gathered at and the invocation
// count of the method at that tick
struct CollectorNode {
  unsigned int num_calls;
  unsigned int collected_at_tick;

  CollectorNode* next;
  CollectorNode* prev;
};

// Creates, stores, and manages a list of nodes
class MethodDataCollector VALUE_OBJ_CLASS_SPEC {
private:
  // Pointers to the head and tail of the linked list
  CollectorNode* headptr;
  CollectorNode* tailptr;

  // Indicates whether the method meets the minimum requirement for compilation
  bool was_likely_compiled;
  // Stores the length of the linked list
  int length;
  
  // Accessors to the front node and tail node for clean consistency 
  CollectorNode* get_first_node();
  CollectorNode* get_last_node();

  // creates a node from data
  CollectorNode* make_node(unsigned int calls, unsigned int tick);

  // removes the first data node from the linked list (not the head node)
  CollectorNode* remove_first_node();

  // Deletes the node
  void destroy_node(CollectorNode& node);
  
  // Appends a node to the end of the list
  void insert(CollectorNode* node);
  
public:  
  MethodDataCollector();
  ~MethodDataCollector();

  bool check_was_likely_compiled() { return was_likely_compiled; }
  int get_length() { return length; }

  // Checks the current last node in the list's count for equality with the given integer input
  bool check_for_count_change(unsigned int current_count);
  
  // Creates and adds a new node to the list 
  void add_node(unsigned int calls, unsigned int tick);

  // Traverses the linked list executing a function on each node as it goes
  void traverse(void (*node_action_function)(CollectorNode&));

  // Writes the collector to a file
  void output_data_collector(OurFileStream* OFS, methodOop m);
};

#endif
