#include <stdio.h>
void printInt(int x) { printf("%d\n", x); }


int main() {
    {
        int var0 = 3;
        int var1;
        
        var0 = 3;
    }
    return 0;
}