#include <stdio.h>

int count = 0;

__attribute__((noinline))
int first(void) {
    count += 1;
    printf("[first]  count = %d\n", count);
    return 10;
}

__attribute__((noinline))
int second(void) {
    count += 10;
    printf("[second] count = %d\n", count);
    return 20;
}

__attribute__((noinline))
void print_sum(int a, int b) {
    printf("Result of a + b: %d\n", a + b);
}

int main(void) {
    printf("Demonstration 1: Function Argument Evaluation Order\n");
    print_sum(first(), second());
    
    count = 0; // reset count

    printf("Demonstration 2: Expression Operand Evaluation Order\n");
    int total = first() + second();
    printf("Total: %d\n", total);

    return 0;
}
