#include <fstream>
#include <string>
#include "precompiled.hpp"
#include "oops/constMethodOop.hpp"
#include "runtime/timer.hpp"

#include "jr_custom_classes/fileIO.hpp"
#include "jr_custom_classes/methodGatherer.hpp"
#include "jr_custom_classes/methodCollector.hpp"

using namespace std;

//prints the compressed method data " tick|total number of calls "
void printMethodData(CollectorNode& node)
{
  tty->print(" %d|%d", node.collected_at_tick, node.num_calls);
}

// prints the difference from the current number of calls to the
// previous node's number of calls in the same format as the function
// above
void printNormalizedMethodData(CollectorNode& node)
{
  unsigned int prev_num;
  if (node.prev == NULL)
    prev_num = 0;
  else 
    prev_num = node.prev->num_calls;
  
  
  tty->print(" %d|%d", node.collected_at_tick, (node.num_calls - prev_num));
}

// prints the total number of calls filling in blank regions with previous data
void printVerboseMethodData(CollectorNode& node)
{
  unsigned int i;
  unsigned int prev_num;
  if (node.prev == NULL) {
    i = 1;
    prev_num = 0;
  }
  else {
    i = node.prev->collected_at_tick + 1;
    prev_num = node.prev->num_calls;
  }
  for (; i < node.collected_at_tick; i++) {
    tty->print(" %d", prev_num);
  }
  tty->print(" %d", node.num_calls);
}

// prints similar to the second print method only adds 0s to fill in
// empty regions
void printVerboseNormalizedMethodData(CollectorNode& node)
{
  unsigned int i;
  unsigned int prev_num;
  if (node.prev == NULL) {
    i = 1;
    prev_num = 0;
  }
  else {
    i = node.prev->collected_at_tick + 1;
    prev_num = node.prev->num_calls;
  }
  for (; i < node.collected_at_tick; i++) {
    tty->print(" 0");
  }
  tty->print(" %d", (node.num_calls - prev_num));
}

// ----- Not Used Anymore -----
// outputs the binary data to a binary csv file 
void output_binary_csv_file(CollectorNode& node, OurFileStream* OFS, bool func_likely_compiled)
{
  unsigned int i;
  unsigned int prev_num;
  int data = node.num_calls - prev_num;
  int wdata;

  if (node.prev == NULL) {
    i = 1;
    prev_num = 0;
  }
  else {
    i = node.prev->collected_at_tick + 1;
    prev_num = node.prev->num_calls;
  }

  for (; i < node.collected_at_tick; i++) {
    /*if (node.prev != NULL && (node.prev->collected_at_tick > node.collected_at_tick - 5 || (data >= CompileThreshold && node.prev->collected_at_tick > node.collected_at_tick - 10))) {
      OFS->write_data(1);
    }
    else {*/
      OFS->write_data(0);
      //}

  }
  
  if (func_likely_compiled) {
    if (data < 1) {
      /*if (node.prev != NULL && node.next != NULL &&			\
	  node.prev->collected_at_tick >= node.collected_at_tick - 3 && \
	  node.next->collected_at_tick <= node.collected_at_tick + 3)
	wdata = 1;
	else*/
	wdata = 0;
    }
    else {
      wdata = 1;
    }
    
    OFS->write_data(wdata);
  }
  else {
    if (data < 1) wdata = 0;
    else wdata = 1;

    OFS->write_data(wdata);
  }
}


// outputs the node data to a file
void output_file(CollectorNode& node, OurFileStream* OFS)
{
  OFS->write_cstring(" ");
  OFS->write_data(node.num_calls - node.prev->num_calls);
  OFS->write_cstring("|");
  OFS->write_data(node.collected_at_tick);
}



// constructs the method data collector
MethodDataCollector::MethodDataCollector()
{
  //collecting_method = m;
  was_likely_compiled = false;
  headptr = make_node(0, 0);
  tailptr = headptr;
  headptr->next = NULL;
  headptr->prev = NULL;
  assert(headptr != NULL, "The headptr does not point to the head node.");
  assert(tailptr != NULL, "The tailptr does not point to the head node.");
}

// destructs the method data collector
MethodDataCollector::~MethodDataCollector()
{
  while (get_first_node() != NULL) {
    destroy_node(*remove_first_node());
  }
  
  delete headptr;
  headptr = NULL;
  tailptr = NULL;
}

// returns the first node
CollectorNode* MethodDataCollector::get_first_node()
{
  return headptr->next;
}

// returns the last node
CollectorNode* MethodDataCollector::get_last_node()
{
  if (tailptr != headptr) {
    return tailptr;
  }
  else {
    return NULL;
  }
}

// creates a new node
CollectorNode* MethodDataCollector::make_node(unsigned int calls, unsigned int tick)
{
  CollectorNode* node = new CollectorNode();
  
  node->num_calls = calls;
  node->collected_at_tick = tick;
  node->next = NULL;
  node->prev = NULL;

  return node;
}

// destroys a node (used by destructor)
void MethodDataCollector::destroy_node(CollectorNode& node) {
  node.next = NULL;
  node.prev = NULL;
  delete &node;
}

// traverses the linked list this collector contains applying
// the node_action_function to each node (usually used for printing)
void MethodDataCollector::traverse(void (*node_action_function)(CollectorNode&))
{
  CollectorNode* node = get_first_node();
  while (node != NULL) {
    (*node_action_function)(*node);
    node = node->next;
  }
}

// un-links the first data node and returns a pointer to it
CollectorNode* MethodDataCollector::remove_first_node()
{
  tty->print_cr("removing node");
  if (get_first_node() != NULL) {
    CollectorNode* node1 = get_first_node();
    CollectorNode* node2 = node1->next;

    node1->next = NULL;
    node1->prev = NULL;
    node2->prev = headptr;

    headptr->next = node2;
    
    return node1;
  }
  else {
    return NULL;
  }
}

// inserts a node to the tail of the linked list
void MethodDataCollector::insert(CollectorNode* node)
{
  assert (tailptr != NULL, "The tailptr was null. Failed to insert.");
  CollectorNode* prev_node = tailptr;
  tailptr->next = node;
  node->prev = tailptr;
  tailptr = node;
  //assert (tailptr == node, "The tailptr was not successfully added for some reason. Failed to insert.");
  //assert (tailptr->prev == prev_node, "failed to insert: this->prev is bad");
  //assert (prev_node->next == node, "failed to insert: prev->next is bad");
}

// returns true if there is a difference in the count from the
// last not to the current one
bool MethodDataCollector::check_for_count_change(unsigned int current_count)
{
  if (get_last_node() == NULL) {
    return true;
  }
  return current_count != get_last_node()->num_calls;
}

// a public function called to add new data to the linked list
void MethodDataCollector::add_node(unsigned int calls, unsigned int tick)
{
  //tty->print_cr("node# %8d : tick# %8d : # of methods %8d", ++count, MethodCallGatherer::_gathererTicks, count2); 
  CollectorNode* node = make_node(calls, tick);
  CollectorNode* test_node = tailptr;
  insert(node);
  
  assert(test_node->next == node, "failed to insert node: next");
  assert(node->prev == test_node || test_node == headptr, "failed to insert node: prev");
  
  length++;
  
  was_likely_compiled = (calls >= (unsigned) CompileThreshold);
}

void MethodDataCollector::output_data_collector(OurFileStream* OFS, methodOop m) 
{
  //HandleMark hm;
  //methodHandle mh(m);
  // flag whether the function was likely_compiled_at some point
  if (check_was_likely_compiled()) {
    OFS->write_cstring("+ ");
  }
  else {
    OFS->write_cstring("- ");
  }

  // output this method's name to the file
  OFS->write_cstring("\"");
  OFS->write_cstring(m->name_and_sig_as_C_string());
  OFS->write_cstring("\"");
   
  // output the total invocation count and the total backedge count for this method
  // over the entire run
  OFS->write_cstring(" {");
  OFS->write_data(m->our_invocation_count());
  OFS->write_cstring(" ");
  OFS->write_data(m->our_backedge_count());
  OFS->write_cstring("}");
  
  // same code as traverse except traverse would require many more arguments
  // for this to work with output_csv_file so I decided to copy and past it here
  //
  // traverses each node and outputs the node's data
  CollectorNode* node = get_first_node(); 
  //assert(node != NULL, "Method has no first node");
  while (node != NULL) {
    
    output_file(*node, OFS);
    //assert(node->prev != NULL, "No prev detected");
    //assert(node->next->prev == node, "Bad pointer");
    //assert(node->prev->next == node, "Bad pointer2");
    node = node->next;
  }
  
  OFS->new_line();
}

