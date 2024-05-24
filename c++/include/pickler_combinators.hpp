#include <boost/function.hpp>
#include <boost/variant.hpp>
#include <vector>

template <typename A>
using pu = boost::function<void(A)>;

pu<int> nat() {
    int half = 128;
    return [half](int n) {
        if (n < half) {
            unit<int>()(n);
        } else {
            wrap(
                [half](int i) { return half + i % half; },
                [half](int i) { return (i - half) / half; }
            )(nat())(n);
        }
    };
}

template <typename A>
pu<std::vector<A>> fixedList(pu<A> pa, int n) {
    if (n == 0) {
        return [](std::vector<A>) {};
    }
    return wrap(
        [](std::pair<A, std::vector<A>> p) {
            std::vector<A> v = p.second;
            v.insert(v.begin(), p.first);
            return v;
        },
        [](std::vector<A> v) {
            A a = v.front();
            v.erase(v.begin());
            return std::make_pair(a, v);
        }
    )(pair(pa, fixedList(pa, n - 1)));
}

template <typename A>
pu<std::vector<A>> list(pu<A> pa) {
    return [pa](std::vector<A> v) {
        int n = v.size();
        nat()(n);
        fixedList(pa, n)(v);
    };
}

pu<std::string> string = list(char_);

template <typename A>
pu<A> alt(boost::function<int(A)> tag, std::vector<pu<A>> ps) {
    if (ps.empty()) {
        throw std::runtime_error("alt: empty list");
    }
    return [tag, ps](A a) {
        int n = tag(a);
        ps[n](a);
    };
}

template <typename A>
pu<boost::optional<A>> pMaybe(pu<A> pa) {
    return alt<boost::optional<A>>(
        [](boost::optional<A> p) { return p ? 1 : 0; },
        {
            unit<boost::optional<A>>(),
            wrap(
                [](A a) { return boost::optional<A>(a); },
                [](boost::optional<A> p) { return p.get(); }
            )(pa)
        }
    );
}

template <typename A, typename B>
pu<boost::variant<A, B>> pEither(pu<A> pa, pu<B> pb) {
    return alt<boost::variant<A, B>>(
        [](boost::variant<A, B> v) { return v.which(); },
        {
            wrap(
                [](A a) { return boost::variant<A, B>(a); },
                [](boost::variant<A, B> v) { return boost::get<A>(v); }
            )(pa),
            wrap(
                [](B b) { return boost::variant<A, B>(b); },
                [](boost::variant<A, B> v) { return boost::get<B>(v); }
            )(pb)
        }
    );
}