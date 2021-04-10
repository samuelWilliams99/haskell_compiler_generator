#include <stdio.h>
void printInt(int x) { printf("%d\n", x); }


int main() {
    {
        int var0 = 0;
        int var1 = 10;
        while((var1) != (0)){
            var0 = (var0) + (1);
            if(((var0) / (2)) != (((var0) - (1)) / (2))){
                printInt(var0);
                var1 = (var1) - (1);
            } else {
            
            }
        }
    }
    return 0;
}