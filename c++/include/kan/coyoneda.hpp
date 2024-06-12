#include <boost/fusion/include/vector.hpp>
#include <boost/fusion/include/make_vector.hpp>
#include <functional>

template <typename F, typename A>
struct Coyoneda {
    std::function<typename F::type(A)> f;
    typename F::type m;

    Coyoneda(std::function<typename F::type(A)> f, typename F::type m)
        : f(f), m(m) {}

    template <typename B>
    Coyoneda<F, B> contramap(std::function<B(A)> g) const {
        return Coyoneda<F, B>(
            [this, g](B b) { return this->f(g(b)); },
            this->m);
    }
};

template <typename F>
struct Contravariant {
    typedef typename F::type type;

    template <typename A, typename B>
    static type contramap(type m, std::function<B(A)> f) {
        return m.contramap(f);
    }
};