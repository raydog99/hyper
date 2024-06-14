#include <boost/function.hpp>
#include <boost/utility.hpp>

template <typename F, typename A>
struct Yoneda {
    typedef boost::function
        typename boost::result_of<F(boost::function<A(void)>)>::type>
        Functor;
    Functor runYoneda;

    template <typename G>
    typename boost::result_of<F(G)>::type lowerYoneda(G g) const {
        return runYoneda(boost::lambda::_1);
    }
};

template <typename F, typename A>
Yoneda<F, A> liftYoneda(
    typename boost::result_of<F(boost::function<A(void)>)>::type fa) {
    return Yoneda<F, A>{
        [fa](boost::function<A(void)> k) -> decltype(fa(k)) { return fa(k); }};
}

template <typename F, typename A, typename B>
Yoneda<F, B> contramap(boost::function<B(A)> g, Yoneda<F, A> m) {
    return Yoneda<F, B>{[m, g](boost::function<B(void)> k) -> decltype(
                              m.runYoneda([k, g](A x) { return k(g(x)); })) {
        return m.runYoneda([k, g](A x) { return k(g(x)); });
    }};
}