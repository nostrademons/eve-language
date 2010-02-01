// Copyright 2010 by Jonathan Tang.
// Lightweight processes in Eve.

#include "heap.h"

typedef struct {
  EveHeap* heap_;
} EveProcess;

EveProcess* eve_new_process();
void eve_destroy_process(EveProcess* process);

EveProcess* eve_get_current_process();
void eve_set_current_process(EveProcess* process);
