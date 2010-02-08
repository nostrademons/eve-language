// Copyright 2010 by Jonathan Tang.

#include "process.h"

#include <stdlib.h>

// TODO: This should really be threadlocal, but gcc on the Mac doesn't support
// it.  This works until we try to go multicore.
static EveProcess* current_process_;

EveProcess* eve_new_process() {
  EveProcess* process = (EveProcess*) malloc(sizeof(EveProcess));
  process->heap_ = eve_new_heap(EVE_DEFAULT_HEAP_SIZE);
}

void eve_destroy_process(EveProcess* process) {
  eve_destroy_heap(process->heap_);
  free(process);
}

EveHeap* eve_process_get_heap(EveProcess* process) {
  return process->heap_;
}

EveProcess* eve_get_current_process() {
  return current_process_;
}

void eve_set_current_process(EveProcess* process) {
  current_process_ = process;
}
