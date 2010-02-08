#ifndef EVE_RUNTIME_HEAP_H
#define EVE_RUNTIME_HEAP_H

#define EVE_DEFAULT_HEAP_SIZE 4000

#include <stdlib.h>

typedef struct {
  void* from_start_;
  void* from_end_;
  void* to_start_;
  void* allocator_;
} EveHeap;

EveHeap* eve_new_heap(size_t heap_size);
void eve_destroy_heap(EveHeap* heap);

void* eve_gcmalloc(EveHeap* heap, size_t num_bytes);

// TODO: The garbage collector.

#endif
