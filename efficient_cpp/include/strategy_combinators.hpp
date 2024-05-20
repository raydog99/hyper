#include <boost/variant.hpp>
#include <functional>

template <typename M, typename A>
struct TP;

template <typename M, typename A>
struct TU;

// Strategy types
template <typename M, typename A>
using Strategy = boost::variant<TP<M, A>, TU<M, A>>;

template <typename M, typename A>
struct TP {
    std::function<A(M)> apply;
};

template <typename M, typename A>
struct TU {
    std::function<A(M)> apply;
};

// Strategy application
template <typename M, typename A>
A applyTP(const TP<M, A>& tp, const M& m) {
    return tp.apply(m);
}

template <typename M, typename A>
A applyTU(const TU<M, A>& tu, const M& m) {
    return tu.apply(m);
}

// Strategy construction
template <typename M, typename A>
TP<M, A> polyTP(const M& m) {
    return TP<M, A>{[m](const M& x) { return x; }};
}

template <typename M, typename A>
TU<M, A> polyTU(const M& m) {
    return TU<M, A>{[m](const M& x) { return x; }};
}

template <typename M, typename A, typename B>
TP<M, B> adhocTP(const M& m, const A& t) {
    return TP<M, B>{[m, t](const M& x) { return m(t); }};
}

template <typename M, typename A, typename B>
TU<M, B> adhocTU(const M& m, const A& t) {
    return TU<M, B>{[m, t](const M& x) { return m(t); }};
}

// Sequential composition
template <typename M, typename A>
TP<M, A> seqTP(const TP<M, TP<M, A>>& tp) {
    return TP<M, A>{[tp](const M& x) { return applyTP(tp.apply(x), x); }};
}

template <typename M, typename A>
TP<M, A> letTP(const TU<M, TP<M, A>>& tu) {
    return TP<M, A>{[tu](const M& x) { return applyTP(tu.apply(x), x); }};
}

template <typename M, typename A>
TU<M, A> seqTU(const TP<M, TU<M, A>>& tp) {
    return TU<M, A>{[tp](const M& x) { return applyTU(tp.apply(x), x); }};
}

template <typename M, typename A>
TU<M, A> letTU(const TU<M, TU<M, A>>& tu) {
    return TU<M, A>{[tu](const M& x) {
        return applyTU(tu.apply(x), applyTU(tu.apply(x), x));
    }};
}

// Choice
template <typename M, typename A>
TP<M, A> choiceTP(const M& m) {
    return TP<M, A>{[m](const M& x) { return m(x); }};
}

template <typename M, typename A>
TU<M, A> choiceTU(const M& m) {
    return TU<M, A>{[m](const M& x) { return m(x); }};
}

// Traversal combinators
template <typename M, typename A>
TP<M, A> allTP(const M& m) {
    return TP<M, A>{[](const M& x) { return x; }};
}

template <typename M, typename A>
TP<M, A> oneTP(const M& m) {
    return TP<M, A>{[](const M& x) { return x; }};
}

template <typename M, typename A>
TU<M, A> allTU(const M& m, const A& a) {
    return TU<M, A>{[a](const M&) { return a; }};
}

template <typename M, typename A>
TU<M, A> oneTU(const M& m) {
    return TU<M, A>{[m](const M&) { return m; }};
}