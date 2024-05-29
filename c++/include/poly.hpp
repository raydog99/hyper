#include <boost/math/tools/polynomial.hpp>
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