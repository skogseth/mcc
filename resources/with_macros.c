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

#ifdef PIZZA
  int b = 0;
#elifndef VALUE
  int b = 1;
#elifdef VALUE
  int b = 2;
#elifdef VALUE
  int b = 3;
#endif 
  
  return a * b * VALUE;
}
