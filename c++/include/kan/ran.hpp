#include <boost/variant.hpp>
#include <functional>

template <typename A>
struct Identity {
    A value;
};

template <typename T, typename G>
struct Adjunction {
    std::function<T(G)> unit;
    std::function<G(T)> right_adjoint;
    std::function<std::function<T(T)>(std::function<T(G)>)> left_adjunct;
    std::function<std::function<G(G)>(std::function<G(T)>)> right_adjunctor;
};

template <typename T, typename G>
struct Composition {
    std::function<boost::variant<T, G>(T)> compose;
    std::function<T(boost::variant<T, G>)> decompose;
};

template <typename T, typename G>
struct Compose {
    boost::variant<T, G> value;
    Compose(T t) : value(t) {}
    template <typename A>
    Compose<A, G> map(std::function<A(T)> f) const {
        return Compose<A, G>{boost::apply_visitor(f, value)};
    }
};

template <typename G, typename H, typename A>
struct Ran {
    std::function<A(std::function<boost::variant<G, A>(H)>)> un_ran;

    template <typename B>
    Ran<G, H, B> map(std::function<B(A)> f) const {
        return Ran<G, H, B>{
            [this, f](std::function<boost::variant<G, B>(H)> k) {
                return f(un_ran([k](H h) { return boost::get<A>(k(h)); }));
            }};
    }

    template <typename B>
    Ran<G, H, B> apply(const Ran<G, H, std::function<B(A)>>& f) const {
        return Ran<G, H, B>{
            [this, f](std::function<boost::variant<G, B>(H)> k) {
                auto c1_f = f.un_ran([k](H h) { return k(h); });
                auto c2_a = un_ran([k](H h) { return k(h); });
                return boost::get<std::function<B(A)>>(c1_f)(boost::get<A>(c2_a));
            }};
    }
};

template <typename G, typename H, typename A>
Ran<G, H, A> pure(A a) {
    return Ran<G, H, A>{
        [a](std::function<boost::variant<G, A>(H)>) { return a; }};
}

template <typename G, typename H, typename T, typename A>
T to_ran(std::function<T(H)> f, const Ran<G, H, A>& ran) {
    return f(ran.un_ran([](H h) { return boost::variant<G, A>(h); }));
}

template <typename G, typename H, typename T, typename A>
T from_ran(std::function<T(Ran<G, H, A>)> s, H h) {
    return s(Ran<G, H, A>{
        [h](std::function<boost::variant<G, A>(H)> k) {
            return boost::get<A>(k(h));
        }});
}

template <typename G, typename T, typename A>
Ran<T, Identity<A>, A> adjoint_to_ran(const Adjunction<T, G>& adj, G g) {
    return Ran<T, Identity<A>, A>{
        [adj, g](std::function<boost::variant<T, A>(Identity<A>)> k) {
            return boost::get<A>(k(Identity<A>{adj.unit(g)}));
        }};
}

template <typename G, typename T, typename A>
T ran_to_adjoint(const Adjunction<T, G>& adj, const Ran<T, Identity<A>, A>& ran) {
    return adj.right_adjoint(to_ran(
        [](Identity<A> i) { return i.value; }, ran));
}

template <typename G, typename T, typename H, typename A>
H ran_to_composed_adjoint(const Adjunction<T, G>& adj, const Ran<T, H, A>& ran) {
    return to_ran(
        [adj](H h) {
            return h.map([adj](T t) { return adj.right_adjoint(t); });
        },
        ran);
}

template <typename G, typename T, typename H, typename A>
Ran<T, H, A> composed_adjoint_to_ran(const Adjunction<T, G>& adj, const H& h) {
    return Ran<T, H, A>{
        [adj, h](std::function<boost::variant<T, A>(H)> k) {
            auto mapped_h = h.map([adj](G g) { return adj.unit(g); });
            return boost::get<A>(k(mapped_h));
        }};
}

template <typename T, typename G, typename H, typename A>
Ran<Compose<T, G>, H, A> compose_ran(const Composition<T, G>& comp, const Ran<T, Ran<G, H, A>, A>& ran) {
    return Ran<Compose<T, G>, H, A>{
        [comp, ran](std::function<boost::variant<Compose<T, G>, A>(H)> k) {
            return ran.un_ran([k, comp](Ran<G, H, A> r) {
                auto composed = r.un_ran([k, comp](H h) {
                    auto result = k(h);
                    return boost::variant<Compose<T, G>, A>{
                        Compose<T, G>{comp.compose(boost::get<T>(result))},
                        boost::get<A>(result)};
                });
                return boost::get<A>(composed);
            });
        }};
}

template <typename T, typename G, typename H, typename A>
Ran<T, Ran<G, H, A>, A> decompose_ran(const Composition<T, G>& comp, const Ran<Compose<T, G>, H, A>& ran) {
    return Ran<T, Ran<G, H, A>, A>{
        [comp, ran](std::function<boost::variant<T, Ran<G, H, A>>(Ran<G, H, A>)> k) {
            return ran.un_ran([k, comp](H h) {
                auto result = k(Ran<G, H, A>{
                    [h, comp](std::function<boost::variant<G, A>(H)> k) {
                        auto composed = k(h);
                        return boost::get<A>(composed);
                    }});
                return boost::variant<T, Ran<G, H, A>>{
                    comp.decompose(boost::get<Compose<T, G>>(result)),
                    boost::get<Ran<G, H, A>>(result)};
            });
        }};
}

template <typename H, typename A>
Ran<Identity<A>, H, A> gran(H h) {
    return Ran<Identity<A>, H, A>{
        [h](std::function<boost::variant<Identity<A>, A>(H)> k) {
            return boost::get<A>(k(h));
        }};
}