#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

typedef void* ml_value_t;

#define ML_UNIT ((ml_value_t) 1)
#define ML_INT(x) ((uintptr_t) x)
#define ML_BLOCK(x) ((ml_value_t*) x)

extern void ml_main();

// runtime primitives

ml_value_t ml_alloc(ml_value_t blsize) {
    return malloc(ML_INT(blsize) * sizeof(ml_value_t));
}

ml_value_t ml_blcopy(ml_value_t dst, ml_value_t src, ml_value_t len) {
    for(int i = 0; i < ML_INT(len); ++i)
        ML_BLOCK(dst)[i] = ML_BLOCK(src)[i];

    return NULL;
}

// builtins

ml_value_t ml_eq(ml_value_t x, ml_value_t y) {
    return (ml_value_t) (uintptr_t) (x == y);
}

ml_value_t ml_add(ml_value_t x, ml_value_t y) {
    return (void*) (((uintptr_t) x) + ((uintptr_t) y));
}

ml_value_t ml_cons(ml_value_t car, ml_value_t cdr) {
    ml_value_t blk = (ml_value_t) ml_alloc((ml_value_t) (uintptr_t) 2);
    ML_BLOCK(blk)[0] = car;
    ML_BLOCK(blk)[1] = cdr;
    return blk;
}

// standard library

ml_value_t ml_fst(ml_value_t lst) {
    return ML_BLOCK(lst)[0];
}

ml_value_t ml_snd(ml_value_t lst) {
    return ML_BLOCK(lst)[1];
}

ml_value_t ml_print_int(ml_value_t x) {
    printf("%d\n", ML_INT(x));
    return ML_UNIT;
}

// call a closure with one argument. used when we don't know statically wether
// this closure accepts several arguments or not (see comments about arity
// in codegen.ml in the compiler)
// TODO: well... implement this through runtime checking. for now, all we do
// is giving a single argument but this will obviously fail if the function
// accept more than one argument and should thus be partially applied
// TODO: this should not be written in C but rather compiler-generated

ml_value_t ml_apply_1(ml_value_t arg, ml_value_t f) {
    ml_value_t (*pc)(ml_value_t, ml_value_t) = ML_BLOCK(f)[0];
    return pc(f, arg);
}

int main() {
    ml_main();
    return 0;
}
