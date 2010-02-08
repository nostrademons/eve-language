#include <stdlib.h>

#include "process.h"
#include "heap.h"
#include "strings.h"

EveString* eve_new_string(int length) {
  EveHeap* heap = eve_process_get_heap(eve_get_current_process());
  EveString* retval = (EveString*) eve_gcmalloc(
      heap, (size_t) length + sizeof(int) + sizeof(char));
  retval->length = length;
  retval->c[length] = '\0';
  return retval;
  return NULL;
}
