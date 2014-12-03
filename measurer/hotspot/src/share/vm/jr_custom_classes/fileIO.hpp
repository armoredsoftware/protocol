#ifndef _DATA_FILE_IO
#define _DATA_FILE_IO

#include <fstream>
#include <string>

// This class serves to allow us to not have to interface with fstream in other classes without
// having to directly include it or have to add a using namespace std in each location
class OurFileStream
{
private:
  std::string filename;
  std::fstream* file_stream;

  OurFileStream();
  ~OurFileStream();

public:
  // used to create a new OurFileStream 
  static OurFileStream* initialize_our_output_stream(std::string);

  // fileIO operations
  std::string get_filename();
  void write_heading();
  void write_cstring(const char*);
  void write_data(int);
  void write_data(int, const char*);
  void new_line();
  void end_our_output_stream();

  // these are our primary file streams, declaring them here
  // will make accessing them easier
  static OurFileStream* memory_fstream;
  static OurFileStream* hot_fstream;
};

// For a lack of a better place to put this I'm placing it here.
// This is used to interface with std::string while input and output remain strictly
// in terms of cstrings. This is easier than using cstrings in my opinion and is useful
// for not having to include string and use the standard namespace when ever I wish to 
// use c++ strings.
//
// I should probably switch it back over to cstrings exclusively
class CStringHolder VALUE_OBJ_CLASS_SPEC {
private:
  std::string holder;

public:
  CStringHolder(const char*);

  void append(const char*);

  void set_holder(const char*);
  const char* get_cstring() const;
};
#endif
