#ifndef EVE_RUNTIME_STRINGS_H
#define EVE_RUNTIME_STRINGS_H

typedef struct {
  int length;
  char c[];
} EveString;

EveString* eve_string_new(int length);
EveString* eve_string_concat(EveString* str1, EveString* str2);

#endif
