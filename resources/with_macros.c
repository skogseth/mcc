#define VALUE 3 * 2

int main(void) {
#ifdef PIZZA
  return 10;
#endif
  
#ifndef VALUE
  int a = 2;
#else
  int a = 10;
#endif
  
  return a * VALUE;
}
