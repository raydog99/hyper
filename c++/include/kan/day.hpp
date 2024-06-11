#include <boost/functional.hpp>
#include <utility>

template <typename F, typename G, typename A>
struct Day {
    F f;
    G g;
    boost::function<A(F, G)> apply;

    Day(F f, G g, boost::function<A(F, G)> apply) : f(std::move(f)), g(std::move(g)), apply(std::move(apply)) {}

    template <typename B>
    auto fmap(std::function<B(A)> f) const {
        return Day<F, G, B>(this->f, this->g, [=](F ff, G gg) { return f(apply(ff, gg)); });
    }
};

template <typename F, typename G, typename A>
auto dayPure(A x) {
    return Day<F, G, A>({}, {}, [=](F, G) { return x; });
}

template <typename F, typename G, typename A, typename B>
auto dayAp(Day<F, G, std::function<B(A)>> day1, Day<F, G, A> day2) {
    return Day<F, G, B>(boost::apply(day1.f, day2.f), boost::apply(day1.g, day2.g),
                        [=](F fa, G gc) { return day1.apply(fa, day1.g)(day2.apply(day1.f, gc)); });
}

template <typename F, typename G, typename H, typename A>
auto dayAssoc(Day<F, Day<G, H, A>, A> day) {
    return Day<Day<F, G, A>, H, A>(
        Day<F, G, A>(day.f.f, day.f.g.f, [=](F ff, G gg) { return day.f.apply(ff, gg); }),
        day.f.g.g, [=](Day<F, G, A> fga, H hc) { return day.f.g.apply(fga.g, hc)(fga.apply(fga.f, fga.g)); });
}

template <typename F, typename G, typename H, typename A>
auto dayDisassoc(Day<Day<F, G, A>, H, A> day) {
    return Day<F, Day<G, H, A>, A>(
        day.f.f, Day<G, H, A>(day.f.g, day.g, [=](G gg, H hh) { return day.apply(Day<F, G, A>(gg, hh.f, hh.apply), hh.g); }),
        [=](F ff, Day<G, H, A> gha) { return day.apply(Day<F, G, A>(ff, gha.f, gha.apply), gha.g); });
}

template <typename F, typename G, typename A>
auto daySwapped(Day<F, G, A> day) {
    return Day<G, F, A>(day.g, day.f, [=](G gg, F ff) { return day.apply(ff, gg); });
}

template <typename F, typename A>
auto dayIntro1(F f) {
    return Day<boost::unit_value_t, F, A>({}, f, [=](boost::unit_value_t, F ff) { return ff; });
}

template <typename G, typename A>
auto dayIntro2(G g) {
    return Day<G, boost::unit_value_t, A>(g, {}, [=](G gg, boost::unit_value_t) { return gg; });
}

template <typename F, typename A>
auto dayElim1(Day<boost::unit_value_t, F, A> day) {
    return day.g;
}

template <typename G, typename A>
auto dayElim2(Day<G, boost::unit_value_t, A> day) {
    return day.f;
}

template <typename F, typename G, typename A, typename B>
auto dayTrans1(std::function<B(A)> f, Day<F, G, A> day) {
    return Day<F, G, B>(day.f, day.g, [=](F ff, G gg) { return f(day.apply(ff, gg)); });
}

template <typename F, typename G, typename A, typename B>
auto dayTrans2(std::function<G(A)> g, Day<F, A, B> day) {
    return Day<F, G, B>(day.f, g(day.g), [=](F ff, G gg) { return day.apply(ff, gg); });
}