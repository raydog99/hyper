#include <boost/any.hpp>
#include <functional>
#include <utility>

namespace invar_day {
    template <typename F, typename G, typename A>
    struct Day {
        F fb;
        G gc;
        std::function<A(const boost::any&, const boost::any&)> bca;
        std::function<std::pair<boost::any, boost::any>(const A&)> abc;

        Day(const F& fb, const G& gc,
            const std::function<A(const boost::any&, const boost::any&)>& bca,
            const std::function<std::pair<boost::any, boost::any>(const A&)>& abc)
            : fb(fb), gc(gc), bca(bca), abc(abc) {}
    };

    template <typename F, typename G, typename A, typename B, typename C>
    Day<F, G, std::pair<B, C>> day(const F& fa, const G& gb) {
        return {fa, gb,
                [](const boost::any& b, const boost::any& c) {
                    return std::make_pair(*boost::any_cast<B>(&b), *boost::any_cast<C>(&c));
                },
                [](const std::pair<B, C>& a) {
                    return std::make_pair(boost::any(a.first), boost::any(a.second));
                }};
    }

    template <typename F, typename G, typename A, typename B, typename C, typename D>
    Day<F, G, B> invmap(const Day<F, G, A>& day,
                        const std::function<B(const A&)>& f,
                        const std::function<D(const C&)>& g) {
        return {day.fb, day.gc,
                [=](const boost::any& b, const boost::any& c) {
                    return f(day.bca(b, *boost::any_cast<C>(&c)));
                },
                [=](const B& a) {
                    auto [b, c] = day.abc(a);
                    return std::make_pair(b, boost::any(g(*boost::any_cast<C>(&c))));
                }};
    }

    template <typename F, typename G, typename H, typename A, typename B, typename C, typename D,
              typename E>
    Day<Day<F, G, D>, H, A> assoc(const Day<F, Day<G, H, C>, A>& day) {
        auto f = [=](const A& a) {
            auto [b, c] = day.abc(a);
            auto [d, e] = day.gc.abc(*boost::any_cast<E>(&c));
            return std::make_pair(
                Day<F, G, D>{day.fb, d,
                              [=](const boost::any& b, const boost::any& c) {
                                  return day.bca(b, c);
                              },
                              [b](const D& a) { return std::make_pair(b, a); }},
                e);
        };
        auto g = [=](const Day<F, G, D>& bc, const boost::any& e) {
            return day.bca(bc.fb, day.gc.bca(bc.gc, *boost::any_cast<E>(&e)));
        };
        return {Day<F, G, D>{day.fb, day.gc.gc, [](const boost::any& b, const boost::any& c) { return *boost::any_cast<D>(&b); },
                             [](const boost::any& c) { return std::make_pair(c, boost::any()); }},
                day.gc.gc, g, f};
    }

    template <typename F, typename G, typename H, typename A, typename B, typename D>
    Day<F, Day<G, H, A>, B> disassoc(const Day<Day<F, G, B>, H, A>& day) {
        auto f = [=](const B& b, const Day<G, H, A>& day) {
            return day.bca(day.abc.second(day.bca(day.fb.abc(b), day.gc)), day.abc.first);
        };
        auto g = [=](const B& b) {
            auto [a, c] = day.abc(day.bca(day.fb.abc(b), day.gc));
            return std::make_pair(b, Day<G, H, A>{day.fb.gc, c, day.fb.bca, a});
        };
        return {day.fb.fb, Day<G, H, A>{day.fb.gc, day.gc, day.fb.bca, day.abc}, f, g};
    }

    template <typename F, typename G, typename A>
    Day<G, F, A> swapped(const Day<F, G, A>& day) {
        return {day.gc, day.fb,
                [=](const boost::any& c, const boost::any& b) {
                    return day.bca(b, c);
                },
                [=](const A& a) {
                    auto [b, c] = day.abc(a);
                    return std::make_pair(c, b);
                }};
    }

    template <typename F, typename A, typename B>
    Day<std::tuple<>, F, B> intro1(const F& fa) {
        return {std::tuple<>{},
                fa,
                [](const boost::any&, const B& a) { return a; },
                [](const B& a) { return std::make_pair(boost::any(std::tuple<>{}), a); }};
    }

    template <typename F, typename A, typename B>
    Day<F, std::tuple<>, B> intro2(const F& fa) {
        return {fa,
                std::tuple<>{},
                [](const B& a, const boost::any&) { return a; },
                [](const B& a) { return std::make_pair(a, boost::any(std::tuple<>{})); }};
    }

    template <typename F, typename A>
    F elim1(const Day<std::tuple<>, F, A>& day) {
        return invmap(day, [](const A& a) { return a; }, [](const auto&) { return std::tuple<>{}; }).gc;
    }

    template <typename F, typename A>
    F elim2(const Day<F, std::tuple<>, A>& day) {
        return invmap(day, [](const A& a) { return a; }, [](const auto&) { return std::tuple<>{}; }).fb;
    }

    template <typename F, typename G, typename H, typename A>
    Day<H, G, A> trans1(const std::function<H(const F&)>& fg, const Day<F, G, A>& day) {
        return {fg(day.fb), day.gc, day.bca, day.abc};
    }

    template <typename F, typename G, typename H, typename A>
    Day<F, H, A> trans2(const std::function<H(const G&)>& gh, const Day<F, G, A>& day) {
        return {day.fb, gh(day.gc), day.bca, day.abc};
    }

    template <typename F, typename G, typename A>
    struct Contravariant {
        F fb;
        G gc;
        std::function<std::pair<boost::any, boost::any>(const A&)> abc;
    };

    template <typename F, typename G, typename A>
    Contravariant<F, G, A> toContravariant(const Day<F, G, A>& day) {
        return {day.fb, day.gc, day.abc};
    }

    template <typename F, typename G, typename A>
    struct Covariant {
        F fb;
        G gc;
        std::function<A(const boost::any&, const boost::any&)> bca;
    };

    template <typename F, typename G, typename A>
    Covariant<F, G, A> toCovariant(const Day<F, G, A>& day) {
        return {day.fb, day.gc, day.bca};
    }

} // namespace invar_day