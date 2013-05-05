/* mode:c; c-basic-offset:4 */
#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<stdarg.h>
#include<assert.h>
#include<stdint.h>
#include<time.h>

#define GEN_TIMING_INFO

#define ARG_SIZE_MAX 5000
#define RET_SIZE_MAX 5000
#define HEAP_SIZE_MAX 1500000

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

////////////////////////////////////////////////////////////////////////
// timing data, all values are in nanoseconds (although we may not
// have such an accuracy).
////////////////////////////////////////////////////////////////////////
#ifdef GEN_TIMING_INFO
struct timespec main_start;
struct timespec main_end;
long main_time;
long gc_time;
#endif

void initialize(void)
{
#ifdef GEN_TIMING_INFO
    clock_gettime(CLOCK_REALTIME, &main_start);
#endif
    
    arg_stack = (closure **)malloc(sizeof(closure *) * ARG_SIZE_MAX);
    ret_stack = (closure **)malloc(sizeof(closure *) * RET_SIZE_MAX);
    heap      = (char *)malloc(HEAP_SIZE_MAX);
}

void finalize(void)
{
    free(arg_stack);
    free(ret_stack);
    free(heap);

#ifdef GEN_TIMING_INFO
    clock_gettime(CLOCK_REALTIME, &main_end);

    main_time += (main_end.tv_sec - main_start.tv_sec) * 1000000000L
        + main_end.tv_nsec - main_start.tv_nsec;

    printf("\n\nTotal time = %.3f seconds\n", main_time / 1.0e9);
    printf("   GC time = %.3f seconds\n", gc_time / 1.0e9);
    printf("  %%GC time = %.3f%%\n", 100.0 * (double)gc_time / main_time);
#endif
}

closure *alloc_heap(size_t nfv)
{
    size_t mysize = sizeof(closure) + sizeof(closure *) * nfv;
    heap_size += mysize;
    assert(heap_size < HEAP_SIZE_MAX);
    return (closure *)(heap + heap_size - mysize);
}

void panic(char *p)
{
    fprintf(stderr, "%s\n", p);
    exit(-1);
}

////////////////////////////////////////////////////////////////////////
// Garbage collection
////////////////////////////////////////////////////////////////////////

closure *run_ubsym(void);
closure *try_move(closure *p);

// Recursively move a closure to its new home and return a pointer to
// the new closure.  We put a redirection struct in the old place so
// any potential references can find its new location.
//
// Upon a successful move of the closure, we set the original closure
// to the following:
//
// 1. Its function pointer code is set to NULL, this serves as a flag
// so we know that it has been moved. A valid closure can not have its
// code pointer equal to NULL.
//
// 2. Its free variable counter fv_cnt is set to the offset of the
// moved closure in the new heap, so we can find the new closure.
//
// As a side note for 2, we can in most cases store directly the
// pointer instead of an offset in fv_cnt. But there is one case I
// found that a data pointer has more bits than size_t.
closure *move_closure(closure *p)
{
    assert(p->code != NULL);

    size_t mysize = sizeof(closure) + sizeof(closure *) * p->fv_cnt;
    size_t offset = heap_size;
    closure *pnew = alloc_heap(p->fv_cnt);

    memcpy(pnew, p, mysize);

    closure **fv_ptr = (closure **)(pnew + 1);

    size_t i;
    for(i = 0; i < p->fv_cnt; ++i) {
        fv_ptr[i] = try_move(fv_ptr[i]);
    }
    p->code = NULL;
    p->fv_cnt = offset;
    return pnew;
}

closure *try_move(closure *p)
{
    if(p->code == NULL) {
        return (closure *)(heap + p->fv_cnt);
    } else if(p->code == run_ubsym) {
        return p;
    } else {
        return move_closure(p);
    }
}

void gc(void)
{
    // I have some ad hoc condition here to test if I need GC.
    if(heap_size * 1.1 <= HEAP_SIZE_MAX) {
        return;
    }

#ifdef GEN_TIMING_INFO
    struct timespec tsc0;
    clock_gettime(CLOCK_REALTIME, &tsc0);
#endif

    char *old_heap = heap;
    heap = (char *)malloc(HEAP_SIZE_MAX);
    if(heap == NULL) {
        panic("Panic: out of memory during gc.");
    }

    heap_size = 0;

    cur_closure = try_move(cur_closure);

    size_t i;
    for(i = 0; i < arg_size; ++i) {
        arg_stack[i] = try_move(arg_stack[i]);
    }
    for(i = 0; i < ret_size; ++i) {
        ret_stack[i] = try_move(ret_stack[i]);
    }

    free(old_heap);

#ifdef GEN_TIMING_INFO
    struct timespec tsc1;
    clock_gettime(CLOCK_REALTIME, &tsc1);

    gc_time += (tsc1.tv_sec - tsc0.tv_sec) * 1000000000L
        + tsc1.tv_nsec - tsc0.tv_nsec;
#endif
}

void push(closure *p)
{
    assert(arg_size < ARG_SIZE_MAX);
    arg_stack[arg_size++] = p;
}

void pop(size_t n)
{
    assert(arg_size >= n);
    arg_size -= n;
}

void push_ret(closure *p)
{
    assert(ret_size < RET_SIZE_MAX);
    ret_stack[ret_size++] = p;
}

closure *my_halt(void)
{
    puts("");
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

#define create_ubsym(name,str)                          \
    static symbol sym_##name = {{run_ubsym, 0}, str};   \
    static closure *name = &(sym_##name.c)

create_ubsym(close_paren, ")");
create_ubsym(param_sep, " ");

void move_arg(void)
{
    size_t i;
    for(i = 0; i < arg_size; ++i) {
        push_ret(arg_stack[i]);
        push_ret(param_sep);
    }
    arg_size = 0;
}

closure *run_ubsym(void)
{
    char *symbol = (char *)(cur_closure + 1);

    if(arg_size > 0) {
        putc('(', stdout);
        push_ret(close_paren);
        move_arg();
    }

    printf("%s", symbol);

    return run_next();
}

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

/* This is used to get the number of args in a __VA_ARGS__ list. */
/* Idea from Stefan Reuther, fe5vsq.17c.1@stefan.msgid.phost.de */
#define PP_NARG(...) (sizeof((closure *[]){__VA_ARGS__})/sizeof(closure *))

#define apply(...)                                                  \
    make_closure(run_apply, PP_NARG(__VA_ARGS__), __VA_ARGS__)

/* We use an gcc extension here! */
/* The ## preceeding __VA_ARGS__ will eat the comma right before it if
   __VA__ARGS__ is empty. */
#define create_closure(name, ...)                                    \
    make_closure(func_##name, PP_NARG(__VA_ARGS__), ##__VA_ARGS__)

#define need_args(n) assert(arg_size >= n)

closure *get_param(size_t i)
{
    return arg_stack[arg_size - 1 - i];
}

#define def_closure(name) \
    closure *func_##name(void)

//////////////////////////////////////////////////////////////////////
