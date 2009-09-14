#ifndef LOCATION_H
#define LOCATION_H

#include <string>

class Location {
 private:
  const char* file_;
  int first_line_;
  int first_column_;
  int last_line_;
  int last_column_;
  
 public:
  // Default constructor; needed for parser.
  Location();
  
  // Copy constructor.  Used to propagate into expression tree.
  Location(const Location& location);
  
  // Move to specified file/line/col, used by the lexer.
  void Advance(const char* file, int line, int first_column, int last_column);
  
  // Initialize from a first and last location, used by the parser.
  void MergeFrom(const Location& first, const Location& last);
  
  // Sets a location to be just after the end of another location.  Used by the
  // parser when there are no RHS tokens.
  void SetAfter(const Location& other);
  
  // Return a string representation.
  std::string ToString() const;
};

#endif