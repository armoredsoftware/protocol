#include <iostream>
#include <fstream>
#include <string>
#include <cstdio>
#include "precompiled.hpp"

#include "jr_custom_classes/methodGatherer.hpp"
#include "jr_custom_classes/fileIO.hpp"

using namespace std;

// Initialize static fields
OurFileStream* OurFileStream::memory_fstream = NULL;//OurFileStream::initialize_our_output_stream("Cold");
OurFileStream* OurFileStream::hot_fstream = NULL;//OurFileStream::initialize_our_output_stream("Hot");

// reads a number char and outputs it as an octal number string
string my_to_string(int num)
{
  string result;
  bool leading_zeros = true;

  for (int i = 15; i >= 0; i -= 3) {
    if (!leading_zeros || ((num >> i) & 0b111) != 0) {
      result.push_back((char)(((num >> i) & 0b111) + 48));
      leading_zeros = false;
    }
  }
  
  return result;
}

// Basic constructor
OurFileStream::OurFileStream() : filename("NULL")
{
  file_stream = new fstream();
}

// Basic Destructor
OurFileStream::~OurFileStream()
{
  delete file_stream;
}

// Create and prepare OurFileStream for writing
OurFileStream* OurFileStream::initialize_our_output_stream(string name)
{
  // create a file stream
  OurFileStream* OFS = new OurFileStream();

  // Generate a filename  
  string path = "/users/f438r398/ProjectData/";
  string type = ".profile";
  int num = 1;
  string total_filename;
  
  if (JRMethodCountTraceOutputFile != NULL)
    total_filename = JRMethodCountTraceOutputFile;
  else
    total_filename = path + name + my_to_string(num) + type;

  OFS->filename = total_filename;

  if (JRMethodCountTraceOutputFile == NULL) {
    OFS->file_stream->open(total_filename.c_str());

    // Ensure that the filename is unique. If it is not unique then generate a new filename one octal
    // number larger
    while (OFS->file_stream->is_open()) {
      OFS->file_stream->close();
      string num_str = my_to_string(++num);
      total_filename = path + name + num_str + type;
      //assert(num < 255, "too many files");
      OFS->file_stream->open(total_filename.c_str());
    }

    // Close the file stream and open it again so that we can overwrite anything in it...
    // There should not be anything in it since it is new...
    OFS->file_stream->close();
  }

  // Notify the user in what file the output is stored
  tty->print_cr("Output Filename: %s", total_filename.c_str());
  tty->cr();

  OFS->file_stream->open(total_filename.c_str(), ios::in | ios::out | ios::trunc);

  return OFS;
}

// write a generic heading to the file
void OurFileStream::write_heading()
{
  write_cstring("Java Command: ");
  write_cstring(Arguments::java_command());
  new_line();
  write_cstring("Compile Threshold: ");
  write_data(CompileThreshold);
  new_line();
  write_cstring("Target Tick Interval: ");
  write_data(JRMethodCountTraceTaskInterval);
  new_line();
  write_cstring("Last Tick: ");
  write_data(MethodCallGatherer::_gathererTicks);
  new_line();
}

// outputs a cstring to the file
void OurFileStream::write_cstring(const char* cstr)
{
  *file_stream << cstr;
}

// outputs a number to the file
void OurFileStream::write_data(int data)
{
  *file_stream << data;
}

// outputs a number to the file in the requested format 
void OurFileStream::write_data(int data, const char* format)
{
  char buffer[21];
  sprintf(buffer, format, data);
  *file_stream << buffer;
}

// Output a new line
void OurFileStream::new_line()
{
  *file_stream << "\n";
}

// Close the output stream and delete it
void OurFileStream::end_our_output_stream()
{
  file_stream->close();
  delete this;
}

// Return the file name
string OurFileStream::get_filename()
{
  return filename;
}

//----------------- CStringHolder Implementation ------------------\\

// Basic Constructor
CStringHolder::CStringHolder(const char* cstr) {
  holder = cstr;
}

// add a cstring to the end of our string
void CStringHolder::append(const char* cstr) {
  holder += cstr;
}

// set the string to a cstring
void CStringHolder::set_holder(const char* cstr)
{
  holder = cstr;
}

// return the string as a cstring
const char* CStringHolder::get_cstring() const
{
  return holder.c_str();
}
