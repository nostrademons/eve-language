// Copyright 2010 by Jonathan Tang.
// Main runtime function for Eve, responsible for starting up the runtime.

// Externally defined by the Eve program.
int eve_main();

int main(int argc, char** argv) {
  // TODO: Marshal argc/argv into an Eve list.
  eve_set_current_process(eve_new_process());
  int retval = eve_main();
  eve_destroy_process(eve_get_current_process());
  return retval;
}
