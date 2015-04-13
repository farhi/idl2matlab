#include <stdio.h>
#include <string.h>

int main(void) {

char* x;
char y[10] = "coucou";

printf("sizeof y=%d\n", sizeof(NULL));
printf("strlen y=%d\n", strlen(NULL));

x = (char*) malloc (strlen(y));


return 0;
}
