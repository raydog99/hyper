#include <boost/utility.hpp>
#include <functional>

template <typename M, typename A>
struct codensity {
    typedef std::function<typename M::template rebind<A>::other(std::function<A(void)>)> codensity_fun;
    codensity_fun fun;

    codensity(codensity_fun f) : fun(f) {}

    static codensity unit(A x) {
        return codensity([x](std::function<A(void)> k) { return M::unit(k()); });
    }

    codensity<M, A> bind(std::function<codensity<M, B>(A)> f) {
        return codensity<M, B>([this, f](std::function<B(void)> k) {
            return this->fun([k, f](A x) {
                return f(x).fun(k);
            });
        });
    }

    template <typename F>
    codensity<M, typename std::result_of<F(A)>::type> map(F f) {
        return codensity<M, typename std::result_of<F(A)>::type>([this, f](std::function<typename std::result_of<F(A)>::type(void)> k) {
            return this->fun([k, f](A x) {
                return M::unit(k(f(x)));
            });
        });
    }

    typename M::template rebind<A>::other lowerCodensity() {
        return fun([](A x) { return M::unit(x); });
    }

    static typename M::template rebind<A>::other reset(codensity<M, A> m) {
        return m.lowerCodensity();
    }

    static codensity<M, A> shift(std::function<codensity<M, A>(std::function<codensity<M, B>(A)>)> f) {
        return codensity<M, A>([f](std::function<A(void)> k) {
            return f([k](A x) {
                return codensity<M, B>([x, k](std::function<B(void)> k2) {
                    return M::unit(k2(x));
                });
            }).fun(k);
        });
    }
};