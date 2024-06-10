#include <boost/functional.hpp>

template <typename G, typename H, typename A>
struct Lift {
    using type = std::function<boost::callable_fw<H, std::function<A, boost::call_traits<G>::value_type>, boost::call_traits<G>::value_type>>;
};

template <typename G, typename H, typename A, typename B, typename C>
auto dimap(std::function<A(B)> f, std::function<C(A)> g, typename Lift<G, H, A>::type m) {
    return [=](auto bac, G a) {
        return m([=](A a) {
            return bac(g(a));
        }, a);
    };
}

template <typename G, typename H, typename A, typename B, typename C>
auto divide(std::function<B(A)> f, typename Lift<G, H, A>::type g, typename Lift<G, H, B>::type h) {
    return [=](auto bac, G a) {
        return h([=](B b) {
            return bac(b);
        }, g([=](A a) {
            return bac(f(a));
        }, a));
    };
}