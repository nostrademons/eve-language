#include <stdlib.h>
#include <stdio.h>
#include <strings.h>

#include "process.h"
#include "heap.h"
#include "strings.h"

EveString* eve_string_new(int length) {
  printf("Creating new string of length %d.\n", length);
  EveHeap* heap = eve_process_get_heap(eve_get_current_process());
  EveString* retval = (EveString*) eve_gcmalloc(
      heap, (size_t) ((length + 1) / sizeof(void*) + 1));
  retval->length = length;
  retval->c[length] = '\0';
  return retval;
}

void eve_string_print(EveString* str) {
  puts(str->c);
}

EveString* eve_string_concat(EveString* str1, EveString* str2) {
  printf("Concatenating %s (%d) and %s (%d).\n",
         &str1->c, str1->length, &str2->c, str2->length);
  EveString* retval = eve_string_new(str1->length + str2->length);
  strncpy(retval->c, str1->c, str1->length);
  strncpy(retval->c + str1->length, str2->c, str2->length);
  return retval;
}
