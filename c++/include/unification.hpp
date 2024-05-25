#include <iostream>
#include <string>
#include <vector>
#include <unordered_map>

using namespace std;

typedef string Id;

struct Term {
    Id id;
    vector<Term> args;
    Term() {}
    Term(const Id& _id) : id(_id) {}
    Term(const Id& _id, const vector<Term>& _args) : id(_id), args(_args) {}
    bool isVar() const { return args.empty(); }
};

typedef unordered_map<Id, Term> Substitution;

bool occurs(const Id& x, const Term& t) {
    if (t.isVar()) return x == t.id;
    for (const Term& arg : t.args)
        if (occurs(x, arg)) return true;
    return false;
}

Term subst(const Term& s, const Id& x, const Term& t) {
    if (t.isVar()) return x == t.id ? s : t;
    vector<Term> newArgs;
    for (const Term& arg : t.args)
        newArgs.push_back(subst(s, x, arg));
    return Term(t.id, newArgs);
}

Term apply(const Substitution& s, const Term& t) {
    if (t.isVar()) {
        auto it = s.find(t.id);
        return it != s.end() ? it->second : t;
    }
    vector<Term> newArgs;
    for (const Term& arg : t.args)
        newArgs.push_back(apply(s, arg));
    return Term(t.id, newArgs);
}

Substitution unifyOne(const Term& s, const Term& t) {
    Substitution sub;
    if (s.isVar()) {
        if (t.isVar()) {
            if (s.id == t.id) return sub;
            sub[s.id] = t;
            return sub;
        }
        if (occurs(s.id, t)) return sub;
        sub[s.id] = t;
        return sub;
    }
    if (t.isVar()) {
        if (occurs(t.id, s)) return sub;
        sub[t.id] = s;
        return sub;
    }
    if (s.id != t.id || s.args.size() != t.args.size()) return sub;
    for (int i = 0; i < s.args.size(); i++) {
        Substitution sub1 = unifyOne(apply(sub, s.args[i]), apply(sub, t.args[i]));
        sub.insert(sub1.begin(), sub1.end());
    }
    return sub;
}

Substitution unify(const vector<pair<Term, Term>>& pairs) {
    Substitution sub;
    for (const auto& p : pairs) {
        Substitution sub1 = unifyOne(apply(sub, p.first), apply(sub, p.second));
        sub.insert(sub1.begin(), sub1.end());
    }
    return sub;
}