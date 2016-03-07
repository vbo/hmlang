#include <stdio.h>
#include <stdint.h>

struct Data {
    int64_t s64_val;
    uint64_t u64_val;
    double float64_val;

    int32_t s32_val;
    uint32_t u32_val;
    float float32_val;

    int16_t s16_val;
    uint16_t u16_val;

    int8_t s8_val;
    uint8_t u8_val;
};

extern "C" {
    int32_t called_from_c(Data *data);
}

int main(int argc, char **argv) {
    printf("\n\n");
    {
        Data data;
        data.s64_val = -64;
        data.u64_val =  64;
        data.float64_val = 64.64;
        data.s32_val = -32;
        data.u32_val =  32;
        data.float32_val = 32.32;
        data.s16_val = -16;
        data.u16_val =  16;
        data.s8_val = -8;
        data.u8_val =  8;
        int32_t r = called_from_c(&data);
        printf("\tout s32: %d\n", r);
        printf("\tout data: {\n");
        printf("\t\ts64: %lld\n", data.s64_val);
        printf("\t\tu64: %llu\n", data.u64_val);
        printf("\t\tfloat64: %f\n", data.float64_val);
        printf("\t\ts32: %d\n", data.s32_val);
        printf("\t\tu32: %u\n", data.u32_val);
        printf("\t\tfloat32: %f\n", data.float32_val);
        printf("\t\ts16: %hd\n", data.s16_val);
        printf("\t\tu16: %hu\n", data.u16_val);
        printf("\t\ts8: %hhd\n", data.s8_val);
        printf("\t\tu8: %hhu\n", data.u8_val);
        printf("\t}\n");
    }
}
