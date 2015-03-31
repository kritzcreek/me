#include <stdio.h>
#include <math.h>

typedef struct {
  float x;
  float y;
} point;

int add_together(int x, int y) {
  int result = x + y;
  return result;
}

int main(int argc, char** argv) {
  point p; p.x = 0.1; p.y = 10.0;
  float length = sqrt(p.x * p.x + p.y * p.y);
  
  int added = add_together(10, 18);

  for (int i = 0; i < 5; i++) {
    puts("Hello, Ben!");
  }

  int i = 0;
  while (i < 10){
    int i_added = add_together(i, added);
    printf("%d plus %d is %d\n", i, added, i_added);
    i++;
  }

  int n = 0;
  do {
    printf("%d) 10 plus 18 is %d\n", n, added);
    n++;
  } while (n < 5);
  
  if (length < 100) {
    puts("The length of the point is less than 100.");
  } else if (length > 100) {
    puts("The length of the point is greater than 100.");
  } else if (length == 100) {
    puts("The length of the point is equal to 100.");
  }
  printf("In fact, it is %f", length);
  return 0;
}
