
extern int preempt_count;

void tcp_twsk_destructor(void) {
  preempt_count--;
}


void inet_twdr_hangman(long data) {
  void (*fn)(void);
  // function pointer
  fn = (void (*)(void)) data; // cast to funptr
  fn();
  // dynamic invocation
}

void __run_timers() {
  long data = (long) &tcp_twsk_destructor;
  int pc = preempt_count; // save
  inet_twdr_hangman(data);
  if (pc != preempt_count) ; // check
}

int main(void) {
  __run_timers();
  return 0;
}
