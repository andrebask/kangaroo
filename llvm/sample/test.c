/* A few global variables */
int globalA;
int globalB = 0;


/* Function prototypes */
void foo(void);
int square(int);

int main(void){
  int localA;
  int localB=2;

  localA = square(localB);
  globalB = localB + 1;

  foo();

  return localA+globalA;

}

void foo(void){
  globalA = globalB + 1;
}

int square(int x){
  return x*x;  
}
