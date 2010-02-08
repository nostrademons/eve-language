#include "heap.h"

#include <stdio.h>
#include <stdlib.h>

EveHeap* eve_new_heap(size_t heap_size) {
  EveHeap* heap = (EveHeap*) malloc(sizeof(EveHeap));
  heap->from_start_ = malloc(heap_size);
  heap->from_end_ = heap->from_start_ + heap_size;
  heap->to_start_ = malloc(heap_size);
  heap->allocator_ = heap->from_start_;
}

void eve_destroy_heap(EveHeap* heap) {
  free(heap->from_start_);
  free(heap->to_start_);
  free(heap);
}

void* eve_gcmalloc(EveHeap* heap, size_t num_words) {
  printf("Allocating %d words at 0x%x\n", num_words, heap->allocator_);
  void* retval = heap->allocator_;
  heap->allocator_ += num_words;
  // TODO: overflow check, GC.
  return retval;
}
