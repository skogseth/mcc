int main(void) {
  int a = 3 * 1;
  int b;
  b = 4 + a - 1;
  int c;

  int d = 0;
  if (b)
    a = 1;
  else
    d = a ? 2 : 3;

  return ~(-2) + a * b - (c = 1);
}
