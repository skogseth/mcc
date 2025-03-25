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

  int j = 1;
  for(int i = 1; i < 10; i = i + 1) j = j * i;

  return ~(-2) + a * b - (c = 1);
}
