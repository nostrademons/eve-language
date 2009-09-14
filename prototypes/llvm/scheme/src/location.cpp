#include "location.h"

#include <sstream>
#include <string>

using std::stringstream;
using std::string;

Location::Location() : file_(NULL), first_line_(0), first_column_(0), 
                       last_line_(0), last_column_(0) {}
                      
Location::Location(const Location& location) : file_(location.file_),
    first_line_(location.first_line_), first_column_(location.first_column_),
    last_line_(location.last_line_), last_column_(location.last_column_) {}

void Location::Advance(const char* file, int line, int first_column, int last_column) {
  file_ = file;
  first_line_ = last_line_ = line;
  first_column_ = first_column;
  last_column_ = last_column;
}

void Location::MergeFrom(const Location& first, const Location& last) {
  file_ = first.file_;
  first_line_ = first.first_line_;
  first_column_ = first.first_column_;
  last_line_ = last.last_line_;
  last_column_ = last.last_column_;
}

void Location::SetAfter(const Location& other) {
  file_ = other.file_;
  first_line_ = last_line_ = other.last_line_;
  first_column_ = last_column_ = other.last_column_;
}

string Location::ToString() const {
  stringstream stream;
  stream << file_ << ":" << first_line_ << "." << first_column_ << "-"
      << last_line_ << "." << last_column_;
  return stream.str();
}