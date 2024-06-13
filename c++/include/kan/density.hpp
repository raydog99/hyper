#include <boost/optional.hpp>
#include <functional>

template <typename A>
class Density {
public:
    Density(std::function<A(std::function<A(A)>)> f) : f_(std::move(f)) {}

    template <typename B>
    Density<B> bind(std::function<Density<B>(A)> k) const {
        auto g = [this, k](std::function<B(B)> h) {
            auto d = k(this->f_(h));
            return d.f_(std::move(h));
        };
        return Density<B>(std::move(g));
    }

    template <typename B>
    Density<B> map(std::function<B(A)> g) const {
        auto f = [this, g](std::function<B(B)> h) {
            auto fh = [h, g](A a) { return h(g(a)); };
            return this->f_(std::move(fh));
        };
        return Density<B>(std::move(f));
    }

    A extract() const {
        return f_([](A a) { return a; });
    }

    Density<Density<A>> duplicate() const {
        auto f = [this](std::function<Density<A>(Density<A>)> g) {
            auto fh = [g, this](A a) {
                auto f2 = [a](std::function<A(A)> h) { return h(a); };
                return g(Density<A>(std::move(f2)));
            };
            return Density<A>(fh);
        };
        return Density<Density<A>>(std::move(f));
    }

    template <typename B>
    Density<B> extend(std::function<B(Density<A>)> g) const {
        auto f = [this, g](std::function<B(B)> h) {
            auto fh = [h, g, this](A a) {
                auto f2 = [a](std::function<A(A)> k) { return k(a); };
                return h(g(Density<A>(f2)));
            };
            return this->f_(std::move(fh));
        };
        return Density<B>(std::move(f));
    }

private:
    std::function<A(std::function<A(A)>)> f_;
};