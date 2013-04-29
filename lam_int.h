// -*- mode:c++ -*-
#include<cassert>
#include<string>
#include<iostream>

struct NForm;
struct Env;

// typedef std::shared_ptr<NForm> NForm_ptr;
typedef NForm* NForm_ptr;
typedef Env *Env_ptr;

struct Env {
    Env_ptr next;
    NForm_ptr nf;
};

NForm_ptr lookupVar(Env_ptr, int n);

enum Tag {UbSym, BSym, Lam, Closure, App, FApp, Thunk};

Env_ptr addBinding(Env_ptr e, NForm_ptr v)
{
    Env_ptr a = new Env;
    a->next = e;
    a->nf = v;
    return a;
}

struct NForm {
    Tag tag;

    const char *s;
    int n;

    NForm_ptr a;
    NForm_ptr b;
    Env_ptr env;

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

    NForm_ptr operator()(Env_ptr e)
    {
        switch(tag) {
        case Lam:
            return NForm_ptr(new NForm(a, e, Closure)); // Closure
        case UbSym:
        case FApp:
            return NForm_ptr(this);
        case BSym:
            return lookupVar(e, n);
        case App:
            {
                NForm_ptr a_val = (*a)(e);
                if(a_val->tag == Closure) {
                    NForm_ptr b_val(new NForm(b, e, Thunk));
                    return (*a_val)(b_val);
                } else {
                    assert(a_val->tag == UbSym);
                    NForm_ptr ret(new NForm(a_val, (*b)(e)));
                    ret->tag = FApp;
                    return ret;
                }
            }
        default:
            assert(false);
            return a;
        }
    }
};

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

#define symbol(sym) NForm_ptr(new NForm(sym))
#define variable(n) NForm_ptr(new NForm(n))
#define lambda(body) NForm_ptr(new NForm(body))
#define apply(a, b) NForm_ptr(new NForm(a, b))
