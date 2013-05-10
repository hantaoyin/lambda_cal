/* mode:c; c-basic-offset:4 */
#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<stdarg.h>
#include<stdint.h>
#include<time.h>

#define GEN_TIMING_INFO

#define ARG_SIZE_MAX 5000
#define RET_SIZE_MAX 5000
#define UPD_SIZE_MAX 50000
#define HEAP_SIZE_MAX 1500000

//#define NDEBUG
#include<assert.h>

struct closure;

typedef struct closure *(*func_ptr)(void);

typedef struct closure {
    func_ptr code;
    size_t fv_cnt;
} closure;

size_t closure_size(closure *p)
{
    return sizeof(closure) + sizeof(closure *) * p->fv_cnt;
}

typedef struct update_frame {
    closure **arg_fp;
    closure **ret_fp;
    closure *updatable;
} update_frame;

// FIXME: flexible array member in struct: a C99 feature!
typedef struct symbol {
    closure c;
    char s[];
} symbol;

closure *cur_closure;
update_frame *upd_base;
closure **arg_base;
closure **ret_base;
closure **arg_fp;
closure **ret_fp;
char *heap;
char *old_heap;

size_t arg_size;
size_t ret_size;
size_t upd_size;
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
unsigned long long total_alloc = 0;
unsigned long long gc_copied = 0;
unsigned long long gc_calls = 0;
#endif

void initialize(void)
{
#ifdef GEN_TIMING_INFO
    clock_gettime(CLOCK_REALTIME, &main_start);
#endif
    
    arg_base = arg_fp = (closure **)malloc(sizeof(closure *) * ARG_SIZE_MAX);
    ret_base = ret_fp = (closure **)malloc(sizeof(closure *) * RET_SIZE_MAX);
    upd_base = (update_frame *)malloc(sizeof(update_frame) * UPD_SIZE_MAX);
    heap      = (char *)malloc(HEAP_SIZE_MAX);
    old_heap  = (char *)malloc(HEAP_SIZE_MAX);
}

void finalize(void)
{
    free(arg_fp);
    free(ret_fp);
    free(upd_base);
    free(heap);
    free(old_heap);

#ifdef GEN_TIMING_INFO
    clock_gettime(CLOCK_REALTIME, &main_end);
    
    main_time += (main_end.tv_sec - main_start.tv_sec) * 1000000000L
        + main_end.tv_nsec - main_start.tv_nsec;
    
    printf("\n\n");
    printf("%20llu bytes allocated in the heap.\n", total_alloc + heap_size);
    printf("%20llu bytes copied during GC.\n", gc_copied);
    printf("%20llu calls to GC.\n", gc_calls);

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
closure *run_indirection(void);

// This function is used when an updatable closure is entered (we mark
// it so it can not be entered again).
closure *black_hole(void)
{
    panic("PANIC: we have entered a black hole, no way back!");
    return NULL;
}

// Move a closure to its new home and return a pointer to the new
// closure.  We put a redirection struct in the old place so any
// potential references can find its new location. The new heap is
// also used as a queue when we iterate through all closures using
// BFS.
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
closure *move(closure *src)
{
    while(src->code == run_indirection) {
        closure **fv_ptr = (closure **)(src + 1);
        src = fv_ptr[0];
    }

    if(src->code == NULL) {
        return (closure *)(heap + src->fv_cnt);
    } else if(src->code == run_ubsym) {
        return src;
    } else {
        size_t mysize = closure_size(src);
        size_t offset = heap_size;
        closure *dst = alloc_heap(src->fv_cnt);

        memcpy(dst, src, mysize);

        src->code = NULL;
        src->fv_cnt = offset;

        return dst;
    }
}

void move_all_bfs(size_t start)
{
    while(start < heap_size) {
        closure *p = (closure *)(heap + start);
        start += closure_size(p);

        if(p->fv_cnt == 0 || p->code == black_hole) continue;

        closure **fv_ptr = (closure **)(p + 1);

        size_t i;
        for(i = 0; i < p->fv_cnt; ++i) {
            fv_ptr[i] = move(fv_ptr[i]);
        }
    }
    assert(start == heap_size);
}

// I have some ad hoc condition here to test if I need GC.
static inline _Bool need_gc(void)
{
    return heap_size * 1.1 > HEAP_SIZE_MAX;
}

void move_upd_frame(void)
{
    closure **arg_pt = arg_base;
    closure **ret_pt = ret_base;

    size_t i = 0;
    size_t j = 0;
    for(i = 0; i < upd_size; ++i) {
        if(arg_pt == NULL) {
            arg_pt = upd_base[i].arg_fp;
            ret_pt = upd_base[i].ret_fp;
        }

        closure *p = upd_base[i].updatable;
        if(p->code == black_hole) continue;
        
        upd_base[j].arg_fp = arg_pt;
        upd_base[j].ret_fp = ret_pt;
        upd_base[j].updatable = move(p);
        ++j;
        arg_pt = NULL;
        ret_pt = NULL;
    }
    upd_size = j;

    if(arg_pt != NULL) {
        arg_size += arg_fp - arg_pt;
        ret_size += ret_fp - ret_pt;
        arg_fp = arg_pt;
        ret_fp = ret_pt;
    }
}

void gc(void)
{
#ifdef GEN_TIMING_INFO
    struct timespec tsc0;
    clock_gettime(CLOCK_REALTIME, &tsc0);

    ++gc_calls;
    total_alloc += heap_size;
#endif

    char *tmp = heap;
    heap = old_heap;
    old_heap = tmp;

    heap_size = 0;

    cur_closure = move(cur_closure);

    size_t i;
    size_t arg_total = arg_size + (arg_fp - arg_base);
    for(i = 0; i < arg_total; ++i) {
        arg_base[i] = move(arg_base[i]);
    }

    size_t ret_total = ret_size + (ret_fp - ret_base);
    for(i = 0; i < ret_total; ++i) {
        ret_base[i] = move(ret_base[i]);
    }

    move_all_bfs(0);
    if(upd_size > 0) {
        move_upd_frame();
    }

#ifdef GEN_TIMING_INFO
    struct timespec tsc1;
    clock_gettime(CLOCK_REALTIME, &tsc1);

    gc_time += (tsc1.tv_sec - tsc0.tv_sec) * 1000000000L
        + tsc1.tv_nsec - tsc0.tv_nsec;

    gc_copied += heap_size;
#endif
}

////////////////////////////////////////////////////////////////////////
// other generic operations
////////////////////////////////////////////////////////////////////////
_Bool need_update(void)
{
    return upd_size > 0;
}

void push(closure *p)
{
    assert(arg_size < ARG_SIZE_MAX);
    arg_fp[arg_size++] = p;
}

void pop(size_t n)
{
    assert(arg_size >= n);
    arg_size -= n;
}

void push_ret(closure *p)
{
    assert(ret_size < RET_SIZE_MAX);
    ret_fp[ret_size++] = p;
}

void push_upd_frame(void)
{
    assert(upd_size < UPD_SIZE_MAX);

    upd_base[upd_size].arg_fp = arg_fp;
    upd_base[upd_size].ret_fp = ret_fp;
    upd_base[upd_size].updatable = cur_closure;

    arg_fp += arg_size;
    ret_fp += ret_size;
    arg_size = ret_size = 0;

    ++upd_size;
}

// pop an update frame
void pop_upd_frame(void)
{
    assert(upd_size > 0);

    --upd_size;
    arg_size += arg_fp - upd_base[upd_size].arg_fp;
    ret_size += ret_fp - upd_base[upd_size].ret_fp;

    arg_fp = upd_base[upd_size].arg_fp;
    ret_fp = upd_base[upd_size].ret_fp;
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
        return ret_fp[--ret_size];
    } else {
        return &halt;
    }
}

#define create_ubsym(name,str)                          \
    static symbol sym_##name = {{run_ubsym, 0}, {str}}; \
    static closure *name = &(sym_##name.c)

create_ubsym(close_paren, ")");
create_ubsym(param_sep, " ");

void move_arg(void)
{
    size_t i;
    for(i = 0; i < arg_size; ++i) {
        push_ret(arg_fp[i]);
        push_ret(param_sep);
    }
    arg_size = 0;
}

// FIXME: push a close_paren object onto the return stack may cause a
// stack overflow if there are too many consecutive close parentheses
// in the output.
//
// How can I solve this problem?
void update_closure(void);
closure *run_ubsym(void)
{
    if(need_update()) {
        update_closure();
        return cur_closure;
    }

    char *symbol = (char *)(cur_closure + 1);

    if(arg_size > 0) {
        putc('(', stdout);
        push_ret(close_paren);
        move_arg();
    }

    printf("%s", symbol);

    return run_next();
}

//#define apply_upd run_apply
#define apply_upd prepare_upd

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

closure *prepare_upd(void)
{
    push_upd_frame();

    closure *p = cur_closure;
    size_t nfv = p->fv_cnt;
    closure **start = (closure **)(p + 1);

    assert(nfv >= 1);

    size_t i;
    for(i = nfv - 1; i > 0; --i) {
        push(start[i]);
    }

    closure *ret = start[0];
    cur_closure->code = black_hole;
    return ret;
}

closure *get_param(size_t i)
{
    return arg_fp[arg_size - 1 - i];
}

//////////////////////////////////////////////////////////////////////

closure *make_upd(closure *ret)
{
    // We either allocate a new space or reuse the existing closure.
    if(ret == NULL) {
        ret = alloc_heap(arg_size + 1);
    }

    ret->code = run_apply;
    ret->fv_cnt = arg_size + 1;

    closure **fv_ptr = (closure **)(ret + 1);
    fv_ptr[0] = cur_closure;

    size_t i;
    for(i = 1; i <= arg_size; ++i) {
        fv_ptr[i] = get_param(i - 1);
    }

    return ret;
}

closure *run_indirection(void)
{
    closure **fv_ptr = (closure **)(cur_closure + 1);
    return fv_ptr[0];
}

// fill a closure with indirection information
void fill_ind_info(closure *src, closure *dst)
{
    assert(src->fv_cnt >= 1);
    src->code = run_indirection;

    closure **fv_ptr = (closure **)(src + 1);
    fv_ptr[0] = dst;
}

void update_closure(void)
{
    assert(ret_size == 0);
    size_t upd_nfv = arg_size + 1;

    closure *old = upd_base[upd_size - 1].updatable;
    if(arg_size == 0) {
        fill_ind_info(old, cur_closure);
    } else if(upd_nfv <= old->fv_cnt) {
        make_upd(old);
    } else { // indirection
        closure *upd = make_upd(NULL);
        fill_ind_info(old, upd);
    }

    pop_upd_frame();
}

// 0. arg_size >= n : nothing happens
//
// 1. arg_size < n && upd_size > 0 : perform an update
//
// 2. arg_size < n && upd_size == 0 : we need to supply some dummy
// variable(s) and print out the current function (not implemented
// yet).
#define need_args(n)                            \
    do {                                        \
        if(arg_size < n) {                      \
            if(need_update()) {                 \
                update_closure();               \
                return cur_closure;             \
            } else {                            \
                assert(0);                      \
            }                                   \
        }                                       \
    } while(0)

