#include <stdio.h>
#include <unistd.h>

void printz()
{
  sleep(1);
}

void bar()
{
  sleep(1);
  printz();
  sleep(1);
}

void foo()
{
  sleep(1);
  bar();
  sleep(1);
}

int main() {
  int a = 0;
  int b = 1;
  int c = 2;
  
  while (1) {
    printf("a=%d b=%d c=%d *c=%d=%08x\n",a,b,c,&c,&c);
    c++;
    bar();
    foo();
    sleep(1);    
  }

}
