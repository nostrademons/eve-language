#define EVE_DEFAULT_HEAP_SIZE 4000

#include <stdlib.h>

typedef struct {
  void* from_start_;
  void* from_end_;
  void* to_start_;
} EveHeap;

EveHeap* eve_new_heap(size_t heap_size);
void eve_destroy_heap(EveHeap* heap);

// TODO: malloc and the garbage collector.
