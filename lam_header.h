// -*- mode:c++; c-basic-offset:4 -*-
#include<iostream>
#include<cassert>
#include<string>
#include<memory>
#include<functional>

struct NForm;
typedef std::shared_ptr<NForm> NForm_ptr;
// typedef NForm* NForm_ptr;

typedef std::function<NForm_ptr (NForm_ptr)> LamFunc;

struct NForm {
    enum {Sym, Lam, App, Thunk} tag;

    const char *sym;
    LamFunc lam;
    NForm_ptr a;
    NForm_ptr b;

    NForm(const char *s):tag(Sym),sym(s) {}

    NForm(LamFunc l):tag(Lam),lam(l) {}

    NForm(NForm_ptr _a, NForm_ptr _b):tag(Thunk),a(_a),b(_b) {}
};

NForm_ptr apply(NForm_ptr a, NForm_ptr b)
{
    return NForm_ptr(new NForm(a, b));
}

NForm_ptr eval(NForm_ptr v)
{
    if(v->tag == NForm::Thunk) {
        NForm_ptr f = eval(v->a);
        NForm_ptr p = v->b;

        if(f->tag == NForm::Lam) {
            *v = *eval(f->lam(p));
        } else {
            v->tag = NForm::App;
        }
    }

    return v;
}

std::string show(NForm_ptr nf)
{
    switch(nf->tag) {
    case NForm::Sym:
        return nf->sym;
    case NForm::Lam:
        return "#FUNC";
    case NForm::Thunk:
        return show(eval(nf));
    default:
        {
            nf->b = eval(nf->b);
            std::string as = nf->a->tag == NForm::Lam ?
                std::string("(") + show(nf->a) + ")" : show(nf->a);
            std::string bs = nf->b->tag == NForm::Sym ?
                show(nf->b) : std::string("(") + show(nf->b) + ")";

            return as + " " + bs;
        }
    }
}

#define lambda(sym, body) \
    NForm_ptr(new NForm([=](NForm_ptr sym) { \
    return body; \
            }))

