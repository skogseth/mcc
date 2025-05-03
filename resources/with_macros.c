#define VALUE 3 * 1

int main(void) {
#ifdef PIZZA
  return 10;
#endif
  
#ifndef VALUE
  return 30;
#endif
  
#ifdef VALUE
  return 100;
#endif
  
  return VALUE;
}
