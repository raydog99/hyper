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