#include <boost/hof.hpp>
#include <functional>

template <typename F, typename G, typename A>
struct lan_f {
    using type = std::function<
        A(std::function<A(F, A)>, G)>;
};

template <typename F, typename G, typename A>
auto fmap = [](auto f, lan_f<F, G, A>::type lan) {
    return [=](auto k, G g) {
        return f(lan([=](F ff, A a) { return k(ff, a); }, g));
    };
};

template <typename F, typename G, typename A, typename B>
auto apply = [](lan_f<F, G, std::function<A(B)>>::type f,
                 lan_f<F, G, B>::type x) {
    return [=](auto k, G g) {
        return f([=](F ff, std::function<A(B)> r) {
                    return [=](B b) { return r(k(ff, b)); };
                 }, g)(x([=](F ff, B b) { return k(ff, b); }, g));
    };
};

template <typename F, typename G, typename A>
auto pure = [](A a) {
    return [=](auto, G) { return a; };
};

template <typename F, typename G, typename A, typename T>
auto to_lan = [](auto phi, lan_f<F, G, A>::type lan) {
    return lan([=](F f, A a) { return phi(f, a); }, boost::hof::identity);
};

template <typename F, typename G, typename A, typename T>
auto from_lan = [](auto phi, G g) {
    return phi([=](auto k, G gg) { return k(boost::hof::identity, gg); }, g);
};