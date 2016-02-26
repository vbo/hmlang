#include <stdio.h>
#include <stdint.h>

extern "C" {
    int32_t main_i32_i32_reti32(int32_t, int32_t);
}

int main(int argc, char **argv) {
    printf("\n\n");
    {
        int32_t arg0 = 40;
        int32_t arg1 = -2;
        int32_t r = main_i32_i32_reti32(arg0, arg1);
        printf("main_i32_i32_reti32(%d, %d):\n", arg0, arg1);
        printf("\t\ti32: %d\n", r);
    }
}
