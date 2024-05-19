#ifndef S_HPP
#define S_HPP

#include <boost/function.hpp>
#include <boost/bind.hpp>

template <typename F, typename G, typename T>
boost::function<T(T)> S(F f, G g) {
    return [f, g](T x) {
        return f(x)(g(x));
    };
}

#endif