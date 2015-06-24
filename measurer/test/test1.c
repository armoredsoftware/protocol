#include <stdio.h>
#include <unistd.h>

int main() {
  int a = 0;
  int b = 1;
  int c = 2;
  
  while (1) {
    printf("a=%d b=%d c=%d *c=%d=%08x\n",a,b,c,&c,&c);
    c++;
    sleep(3);    
  }

}
