// -*- mode:c++ -*-
#include<cassert>
#include<string>
#include<vector>
#include<iostream>

struct NForm;
struct Env;

typedef NForm* NForm_ptr;
typedef Env *Env_ptr;

const size_t env_size = 100000000;
const size_t  nf_size = 100000000;

struct Env {
    Env_ptr next;
    NForm_ptr nf;
};

class EnvMem {
    Env* mem;
    size_t id;
    size_t size;

public:
    EnvMem(size_t _size):id(0),size(_size) {
        mem = new Env[size];
    }
    ~EnvMem() {
        delete[] mem;
    }

    Env *alloc() {
        assert(id + 1 < size);
        ++id;
        return mem + id - 1;
    }
} env_mem(env_size);

NForm_ptr lookupVar(Env_ptr, int n);

enum Tag {UbSym, BSym, Lam, Closure, App, FApp, Thunk};

Env_ptr addBinding(Env_ptr e, NForm_ptr v)
{
    Env_ptr a = new(env_mem.alloc()) Env;
    a->next = e;
    a->nf = v;
    return a;
}

struct NForm {
    Tag tag;

    NForm_ptr a;
    union {
        const char *s;
        int n;
        NForm_ptr b;
        Env_ptr env;
    };

    NForm() {}

    NForm(const char *v):tag(UbSym),s(v) {}

    NForm(int _n):tag(BSym),n(_n) {}

    NForm(NForm_ptr body):tag(Lam),a(body) {
    }

    NForm(NForm_ptr _a, Env_ptr _e, Tag _tag):tag(_tag),a(_a),env(_e)
    {
        assert(tag == Closure || tag == Thunk);
    }

    NForm(NForm_ptr _a, NForm_ptr _b):tag(App),a(_a),b(_b) {
    }

    // only for Closure
    NForm_ptr operator()(NForm_ptr param)
    {
        assert(tag == Closure);
        Env_ptr new_env = addBinding(env, param);
        return (*a)(new_env);
    }

    NForm_ptr operator()(Env_ptr e);
};

class NFormMem {
    NForm *mem;
    size_t id;
    size_t size;

public:
    NFormMem(size_t _size):id(0),size(_size) {
        mem = new NForm[size];
    }
    ~NFormMem() {
        delete[] mem;
    }

    NForm *alloc() {
        assert(id + 1 < size);
        ++id;
        return mem + id - 1;
    }
} nf_mem(nf_size);

inline NForm_ptr NForm::operator()(Env_ptr e)
{
    switch(tag) {
    case Lam:
        return NForm_ptr(new(nf_mem.alloc()) NForm(a, e, Closure)); // Closure
    case UbSym:
    case FApp:
        return NForm_ptr(this);
    case BSym:
        return lookupVar(e, n);
    case App:
        {
            NForm_ptr a_val = (*a)(e);
            if(a_val->tag == Closure) {
                NForm_ptr b_val(new(nf_mem.alloc()) NForm(b, e, Thunk));
                return (*a_val)(b_val);
            } else {
                assert(a_val->tag == UbSym);
                NForm_ptr ret(new(nf_mem.alloc()) NForm(a_val, (*b)(e)));
                ret->tag = FApp;
                return ret;
            }
        }
    default:
        assert(false);
        return a;
    }
}

NForm_ptr lookupVar(Env_ptr e, int n)
{
    for(int i = 0; i < n; ++i) {
        assert(e != NULL);
        e = e->next;
    }
    assert(e != NULL);

    NForm_ptr ret = e->nf;

    if(ret->tag == Thunk) {
        e->nf = (*ret->a)(ret->env);
        return e->nf;
    } else {
        return ret;
    }
}

std::string show(Env_ptr e, NForm_ptr v)
{
    NForm_ptr nf = (*v)(e);
    switch(nf->tag) {
    case UbSym:
        return std::string(nf->s);
    case Lam:
        return std::string("#FUNC");
    case FApp:
        {
            assert(nf->a->tag != Lam);
            std::string as = show(e, nf->a);
            std::string bs = nf->b->tag == FApp ?
                std::string("(") + show(e, nf->b) + ")" : show(e, nf->b);
            return as + " " + bs;
        }
    default:
        assert(false);
        return "";
    }
}

NForm_ptr symbol(const char *sym) {
    return new(nf_mem.alloc()) NForm(sym);
}

NForm_ptr variable(int n) {
    return new(nf_mem.alloc()) NForm(n);
}

NForm_ptr lambda(NForm_ptr body) {
    return new(nf_mem.alloc()) NForm(body);
}

NForm_ptr apply(NForm_ptr a, NForm_ptr b) {
    return new(nf_mem.alloc()) NForm(a, b);
}
