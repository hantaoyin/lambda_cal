#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<stdarg.h>
#include<assert.h>
#include<stdint.h>

#define ARG_SIZE_MAX 500
#define RET_SIZE_MAX 500
#define HEAP_SIZE_MAX 1000000000

struct closure;

typedef struct closure *(*func_ptr)(void);

typedef struct closure {
    func_ptr code;
    size_t fv_cnt;
} closure;

// FIXME: flexible array member in struct: a C99 feature!
typedef struct symbol {
    closure c;
    char s[];
} symbol;

closure *cur_closure;
closure **arg_stack;
closure **ret_stack;
char *heap;

size_t arg_size;
size_t ret_size;
size_t heap_size;

void initialize(void)
{
    arg_stack = (closure **)malloc(sizeof(closure *) * ARG_SIZE_MAX);
    ret_stack = (closure **)malloc(sizeof(closure *) * RET_SIZE_MAX);
    heap      = (char *)malloc(HEAP_SIZE_MAX);
}

void finalize(void)
{
    free(arg_stack);
    free(ret_stack);
    free(heap);
}

void push(closure *p)
{
    assert(arg_size < ARG_SIZE_MAX);
    arg_stack[arg_size++] = p;
}

closure *pop(void)
{
    assert(arg_size > 0);
    return arg_stack[--arg_size];
}

closure *alloc_heap(size_t nfv)
{
    size_t mysize = sizeof(closure) + sizeof(closure *) * nfv;
    heap_size += mysize;
    assert(heap_size < HEAP_SIZE_MAX);
    return (closure *)(heap + heap_size - mysize);
}

closure *my_halt(void)
{
    puts("");
    printf("maximum heap used = %lu\n", heap_size);
    finalize();
    exit(0);
    return NULL;
}

static closure halt = {my_halt, 0};

closure *run_next(void)
{
    if(ret_size != 0) {
        return ret_stack[--ret_size];
    } else {
        return &halt;
    }
}

void move_arg(void)
{
    if(arg_size > 0) {
        memcpy(ret_stack + ret_size,
               arg_stack,
               sizeof(func_ptr) * arg_size);
        
        ret_size += arg_size;
        arg_size = 0;
    }
}

closure *run_ubsym(void)
{
    char *symbol = (char *)(cur_closure + 1);

    size_t i;
    for(i = 0; i < arg_size; ++i) {
        putc('*', stdout);
    }
    move_arg();
    printf("%s ", symbol);
    return run_next();
}

#define create_ubsym(x) \
    static symbol sym_##x = {{run_ubsym, 0}, #x};  \
    static closure *x = &(sym_##x.c)

closure *make_closure(func_ptr func, size_t n, ...)
{
    closure *ret = alloc_heap(n);
    ret->code = func;
    ret->fv_cnt = n;

    closure **fv_ptr = (closure **)(ret + 1);
    va_list args;
    va_start(args, n);

    size_t i;
    for(i = 0; i < n; ++i) {
        fv_ptr[i] = va_arg(args, closure *);
    }
    va_end(args);

    return ret;
}

closure *run_apply(void)
{
    size_t nfv = cur_closure->fv_cnt;
    closure **start = (closure **)(cur_closure + 1);

    assert(nfv >= 1);

    size_t i;
    for(i = nfv - 1; i > 0; --i) {
        push(start[i]);
    }
    return start[0];
}

#ifndef __VA_NARG__
/* This is used to get the number of args in a __VA_ARGS__ list. */
/* Idea from Stefan Reuther, fe5vsq.17c.1@stefan.msgid.phost.de */
#define __VA_NARG__(...) (sizeof((closure *[]){__VA_ARGS__})/sizeof(closure *))
#endif

#define apply(...)                                                  \
    make_closure(run_apply, __VA_NARG__(__VA_ARGS__), __VA_ARGS__)

/* We use an gcc extension here! */
/* The ## preceeding __VA_ARGS__ will eat the comma right before it if
   __VA__ARGS__ is empty. */
#define create_closure(name, ...)                                    \
    make_closure(func_##name, __VA_NARG__(__VA_ARGS__), ##__VA_ARGS__)

void need_args(size_t n) {
    assert(arg_size >= n);
}

#define def_closure(name) \
    closure *func_##name(void)

//////////////////////////////////////////////////////////////////////
