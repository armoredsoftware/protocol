/*
 * Copyright (c) 2003, 2010, Oracle and/or its affiliates. All rights reserved.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * This code is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 only, as
 * published by the Free Software Foundation.
 *
 * This code is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * version 2 for more details (a copy is included in the LICENSE file that
 * accompanied this code).
 *
 * You should have received a copy of the GNU General Public License version
 * 2 along with this work; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA.
 *
 * Please contact Oracle, 500 Oracle Parkway, Redwood Shores, CA 94065 USA
 * or visit www.oracle.com if you need additional information or have any
 * questions.
 *
 */

#include "precompiled.hpp"
#include "oops/constMethodOop.hpp"
#include "oops/methodOop.hpp"

//JG - Change Start
#include "jr_custom_classes/memoryCollector.hpp"
//JG - Change End

// Static initialization
const u2 constMethodOopDesc::MAX_IDNUM   = 0xFFFE;
const u2 constMethodOopDesc::UNSET_IDNUM = 0xFFFF;

// How big must this constMethodObject be?

int constMethodOopDesc::object_size(int code_size,
                                    int compressed_line_number_size,
                                    int local_variable_table_length,
                                    int checked_exceptions_length) {
  int extra_bytes = code_size;
  if (compressed_line_number_size > 0) {
    extra_bytes += compressed_line_number_size;
  }
  if (checked_exceptions_length > 0) {
    extra_bytes += sizeof(u2);
    extra_bytes += checked_exceptions_length * sizeof(CheckedExceptionElement);
  }
  if (local_variable_table_length > 0) {
    extra_bytes += sizeof(u2);
    extra_bytes +=
              local_variable_table_length * sizeof(LocalVariableTableElement);
  }
  int extra_words = align_size_up(extra_bytes, BytesPerWord) / BytesPerWord;
  return align_object_size(header_size() + extra_words);
}


// linenumber table - note that length is unknown until decompression,
// see class CompressedLineNumberReadStream.

u_char* constMethodOopDesc::compressed_linenumber_table() const {
  // Located immediately following the bytecodes.
  assert(has_linenumber_table(), "called only if table is present");
  return code_end();
}

u2* constMethodOopDesc::checked_exceptions_length_addr() const {
  // Located at the end of the constMethod.
  assert(has_checked_exceptions(), "called only if table is present");
  return last_u2_element();
}

u2* constMethodOopDesc::localvariable_table_length_addr() const {
  assert(has_localvariable_table(), "called only if table is present");
  if (has_checked_exceptions()) {
    // If checked_exception present, locate immediately before them.
    return (u2*) checked_exceptions_start() - 1;
  } else {
    // Else, the linenumber table is at the end of the constMethod.
    return last_u2_element();
  }
}


// Update the flags to indicate the presence of these optional fields.
void constMethodOopDesc::set_inlined_tables_length(
                                              int checked_exceptions_len,
                                              int compressed_line_number_size,
                                              int localvariable_table_len) {
  // Must be done in the order below, otherwise length_addr accessors
  // will not work. Only set bit in header if length is positive.
  assert(_flags == 0, "Error");
  if (compressed_line_number_size > 0) {
    _flags |= _has_linenumber_table;
  }
  if (checked_exceptions_len > 0) {
    _flags |= _has_checked_exceptions;
    *(checked_exceptions_length_addr()) = checked_exceptions_len;
  }
  if (localvariable_table_len > 0) {
    _flags |= _has_localvariable_table;
    *(localvariable_table_length_addr()) = localvariable_table_len;
  }
}


int constMethodOopDesc::checked_exceptions_length() const {
  return has_checked_exceptions() ? *(checked_exceptions_length_addr()) : 0;
}


CheckedExceptionElement* constMethodOopDesc::checked_exceptions_start() const {
  u2* addr = checked_exceptions_length_addr();
  u2 length = *addr;
  assert(length > 0, "should only be called if table is present");
  addr -= length * sizeof(CheckedExceptionElement) / sizeof(u2);
  return (CheckedExceptionElement*) addr;
}


int constMethodOopDesc::localvariable_table_length() const {
  return has_localvariable_table() ? *(localvariable_table_length_addr()) : 0;
}


LocalVariableTableElement* constMethodOopDesc::localvariable_table_start() const {
  u2* addr = localvariable_table_length_addr();
  u2 length = *addr;
  assert(length > 0, "should only be called if table is present");
  addr -= length * sizeof(LocalVariableTableElement) / sizeof(u2);
  return (LocalVariableTableElement*) addr;
}

//JG - Change Start
// I am adding this tool here as opposed to methodOop since methodOop seems to cause
// collection problems when accessed concurrent to a GC. Hopefully constMethodOop will
// avoid this.

int constMethodOopDesc::osr_c1_compile_size = 0;
int constMethodOopDesc::reg_c1_compile_size = 0;
int constMethodOopDesc::nat_compile_size = 0;

int constMethodOopDesc::num_recompiles = 0;

// adds the nmethod size to its respective category if the nmethod is unique
// with respect to its category
bool constMethodOopDesc::profile_code(constMethodOop cm, nmethod* code) {
  // This will become true if an nmethod is not unique. I have observed that for
  // OSRs this may not necessarily mean the previous one was deleted. Nevertheless
  // I am not counting multiple OSRs due to the fact that it COULD have been deleted.
  bool recompile_detected = false;

  if (code->is_native_method()) {
    // count a native method
    cm->has_nat_compile = true;
    assert(code->insts_size() != 0, "No code size in native method?");
    nat_compile_size += code->size();
  }
  else if (!cm->has_nat_compile) { // Native methods should not be overwritten, right?
    if (code->is_osr_method()) {
      if (!cm->has_osr_c1_compile && code->is_compiled_by_c1()) {
	// count an osr c1 compile
	cm->has_osr_c1_compile = true;
	osr_c1_compile_size += code->size();
      }
      else {
	recompile_detected = true;
      }
    }
    else if (code->is_nmethod()) { // Should always be true unless there is a segmentation fault
      if (!cm->has_reg_c1_compile && code->is_compiled_by_c1()) {
	// count a regular c1 compile
	cm->has_osr_c1_compile = true;
	reg_c1_compile_size += code->size();
      }
      else {
	recompile_detected = true;
      }
    }
  }
  else {
    // If our assumption that native code is not overwritten fails we will be notified here.
    // May cause us to miss some profiling if the assumption is incorrect
    tty->print_cr("\nNote: Native code can infact be overwritten.\n");
  }

  // make sure this was not compiled by another compiler
  if (!code->is_compiled_by_c1() && !code->is_native_method()) {
    tty->print_cr("\nWarning: Our memory profile is incomplete. \n Alternate compiler %d detected!\n", (code->is_native_method() << 5 | code->is_compiled_by_c1() << 2 | code->is_compiled_by_c2() << 1 | code->is_compiled_by_shark()));
    return false;
  }

  // count the number of duplicate nmethods appear without adding them to the collective
  // nmethod size
  if (recompile_detected) {
    num_recompiles++;
  }
  else {
    // Add method to MethodMemoryProfiler
    MethodMemoryProfiler::add_nmethod(code);
  }

  return recompile_detected;
}

// Prints our data to the terminal
void constMethodOopDesc::print_memory_stats() {
  tty->print_cr("========== Total Compiled Method Memory Profile ============");
  tty->print_cr("C1 compiler:");
  tty->print_cr("  OSRs occupy:                %10d B", get_osr_c1_compile_size());
  tty->print_cr("  Native methods occupy:      %10d B", get_nat_compile_size());
  tty->print_cr("  Regular compiles occupy:    %10d B", get_reg_c1_compile_size());
  tty->cr();
  tty->print_cr("General Stats:");
  tty->print_cr("  Total space occupied:       %10d B", (get_osr_c1_compile_size() +
						     get_nat_compile_size()     +
						     get_reg_c1_compile_size()));
  tty->print_cr("  Number of recompiles:       %10d  ", get_num_recompiles());
  tty->print_cr("============================================================");
}
//JG - Change Start