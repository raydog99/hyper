#include <boost/function.hpp>
#include <boost/utility/result_of.hpp>
#include <functional>

template <typename F, typename A>
struct Coyoneda {
    typedef boost::function
        typename boost::result_of<F(A)>::type()>
        FType;
    typedef boost::function
        boost::shared_ptr<FType>(const std::function<A(void)>&)>
        RunCoyonedaType;

    RunCoyonedaType runCoyoneda;

    template <typename B>
    Coyoneda<std::function<B(A)>, B> contramap(const std::function<A(B)>& f) const {
        return Coyoneda<std::function<B(A)>, B>(
            [this, f](const std::function<B(void)>& k) {
                return this->runCoyoneda([k, f](const std::function<A(void)>& g) {
                    return boost::make_shared<std::function<B(void)>>(
                        [k, f, g]() { return k(f(g())); });
                });
            });
    }

    static Coyoneda<F, A> lift(const F& x) {
        return Coyoneda<F, A>([x](const std::function<A(void)>& f) {
            return boost::make_shared<FType>(
                std::bind(std::function<typename boost::result_of<F(A)>::type(A)>(x), f));
        });
    }

    FType lower() const {
        return this->runCoyoneda(
            [](const std::function<A(void)>& f) {
                return boost::make_shared<FType>(f);
            });
    }

    template <typename G>
    Coyoneda<std::function<typename boost::result_of<G(typename boost::result_of<F(A)>::type)>::type(A)>, A>
    hoist(const boost::function<boost::shared_ptr<typename boost::result_of<G(FType)>::type>(const FType&)>& f) const {
        return Coyoneda<std::function<typename boost::result_of<G(typename boost::result_of<F(A)>::type)>::type(A)>, A>(
            [this, f](const std::function<A(void)>& k) {
                return f(*this->runCoyoneda([k](const std::function<A(void)>& g) {
                    return boost::make_shared<FType>(
                        std::bind(std::function<typename boost::result_of<F(A)>::type(A)>(g), k));
                }));
            });
    }
};

typedef std::function<int(void)> Presheaf;

template <typename A>
Presheaf contramap(const std::function<A(void)>& f, const Presheaf& g) {
    return std::bind(g, f);
}

template <typename A>
Presheaf tabulate(const A& a) {
    return std::function<int(void)>(0);
}