#ifndef EVE_RUNTIME_STRINGS_H
#define EVE_RUNTIME_STRINGS_H

typedef struct {
  int length;
  char c[];
} EveString;

EveString* eve_new_string(int length);

#endif
