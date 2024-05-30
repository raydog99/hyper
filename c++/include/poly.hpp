#include <boost/math/tools/polynomial.hpp>
#include <boost/operators.hpp>
#include <vector>

using namespace boost::math::tools;

template <typename T>
polynomial<T> create_poly(const std::vector<T>& coeffs) {
    return polynomial<T>(coeffs);
}

template <typename T>
T eval_poly(const polynomial<T>& p, T x) {
    return p(x);
}

template <typename T, typename UnaryFunc>
polynomial<T> map_poly(const polynomial<T>& p, UnaryFunc f) {
    std::vector<T> coeffs(p.size());
    for (std::size_t i = 0; i < p.size(); ++i) {
        coeffs[i] = f(p[i]);
    }
    return create_poly(coeffs);
}

template <typename T>
polynomial<T> sum_polys(const polynomial<T>& p, const polynomial<T>& q) {
    return p + q;
}

template <typename T>
polynomial<T> product_polys(const polynomial<T>& p, const polynomial<T>& q) {
    return p * q;
}

template <typename T>
struct PolyTerm {
    T coeff;
    int exp;
};

template <typename T>
using Poly = std::vector<PolyTerm<T>>;

template <typename T>
class PolyComonoid : boost::operators<PolyComonoid<T>> {
public:
    PolyComonoid(const Poly<T>& p) : poly(p) {}

    void counit() const {
        if (poly.empty()) {
            return;
        }
        if (poly[0].exp == 0) {
            poly[0].coeff.counit();
        } else {
            throw std::runtime_error("Polynomial must be constant to have a counit");
        }
    }

    std::pair<Poly<T>, Poly<T>> comult() const {
        Poly<T> xs, ys;
        for (const auto& term : poly) {
            if (term.exp == 0) {
                auto [c1, c2] = term.coeff.comult();
                xs.push_back({c1, 0});
                ys.push_back({c2, 0});
            } else {
                xs.push_back(term);
                ys.push_back({term.coeff.counit(), term.exp});
            }
        }
        return {xs, ys};
    }

private:
    Poly<T> poly;
};