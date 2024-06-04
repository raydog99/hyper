#include <boost/function.hpp>

template <typename Pos, typename Dis>
struct Arena {
    Pos pos;
    boost::function<Dis(Pos)> dis;
};

template <typename Dom, typename Cod>
struct Lens {
    boost::function<Cod(Dom)> observe;
    boost::function<boost::function<Dom(Dom)>(Dom, boost::function<Cod(Cod)>)> interpret;
};

template <typename A>
Lens<A, A> idLens(const Arena<A, A>& a) {
    return Lens<A, A>{
        [](A x) { return x; },
        [](A p, boost::function<A(A)> f) { return f; }
    };
}

template <typename A1, typename A2, typename A3>
Lens<A1, A3> operator<.>(const Lens<A2, A3>& lens23, const Lens<A1, A2>& lens12) {
    auto obs = [&](A1 p) { return lens23.observe(lens12.observe(p)); };
    auto int_ = [&](A1 p, boost::function<A3(A3)> f) {
        return lens12.interpret(p, [&](A2 x) {
            return lens23.interpret(x, [&](A3 y) { return f(y); });
        });
    };
    return Lens<A1, A3>{obs, int_};
}

template <typename Pos, typename Dis>
using Display = std::pair<Pos, boost::function<Dis(Pos)>>;

template <typename Pos, typename Dis, typename Res>
using AsFunctor = std::pair<Pos, boost::function<Res(boost::function<Dis(Pos)>)>>;

template <typename A1, typename A2, typename B1, typename B2>
Lens<std::pair<boost::variant<A1, A2>, boost::variant<B1, B2>>,
     boost::variant<std::pair<A1, B1>, std::pair<A2, B2>>>
duoidal(const A1& a1, const A2& a2, const B1& b1, const B2& b2) {
    using X = std::pair<boost::variant<A1, A2>, boost::variant<B1, B2>>;
    using Y = boost::variant<std::pair<A1, B1>, std::pair<A2, B2>>;

    auto x = std::make_pair(boost::variant<A1, A2>(a1, a2), boost::variant<B1, B2>(b1, b2));
    auto y = boost::variant<std::pair<A1, B1>, std::pair<A2, B2>>(std::make_pair(a1, b1), std::make_pair(a2, b2));

    auto o = [](const X& p) {
        auto p1 = p.first;
        auto p2 = p.second;
        auto q1 = boost::get<A1>(p1);
        auto q2 = boost::get<B1>(p2);

        auto pp = std::make_pair(std::make_pair(q1, q2), [p1, p2](const Dis<std::pair<A1, B1>>& d) {
            auto d1 = d.first;
            auto d2 = d.second;
            return std::make_pair(boost::apply_visitor(p1, d1), boost::apply_visitor(p2, d2));
        });

        return pp;
    };

    auto i = [](const X& p, const Dis<Y>& d) {
        auto p1 = p.first;
        auto p2 = p.second;
        auto q1 = boost::get<A1>(p1);
        auto q2 = boost::get<B1>(p2);

        auto de1 = boost::get<std::pair<A1, B1>>(d);
        auto de2 = boost::get<std::pair<A2, B2>>(d);

        return std::make_pair(std::make_pair(de1.first, de2.first), std::make_pair(de1.second, de2.second));
    };

    return Lens<X, Y>(o, i);
}