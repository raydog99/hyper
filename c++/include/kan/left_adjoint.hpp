#include <boost/fusion/include/vector.hpp>
#include <boost/fusion/include/make_vector.hpp>
#include <boost/fusion/include/as_vector.hpp>
#include <boost/fusion/include/at.hpp>
#include <boost/fusion/include/size.hpp>
#include <boost/fusion/include/transform.hpp>
#include <boost/fusion/include/mpl.hpp>
#include <boost/fusion/include/for_each.hpp>
#include <functional>
#include <vector>

template <typename F, typename G, typename A>
struct LeftAdjoint {
    typedef std::function<boost::fusion::vector<A>(boost::fusion::vector<std::function<A(boost::fusion::vector<A>)>>)> F_type;
    typedef std::function<boost::fusion::vector<G>(boost::fusion::vector<G>)> G_type;

    F_type f;
    G_type g;

    LeftAdjoint(F_type f, G_type g) : f(f), g(g) {}

    template <typename B>
    LeftAdjoint<F, G, B> fmap(std::function<B(A)> f) {
        return LeftAdjoint<F, G, B>(
            [f, this](boost::fusion::vector<std::function<B(boost::fusion::vector<A>)>> vec) {
                return boost::fusion::transform(this->f(boost::fusion::transform(vec, [f](std::function<A(boost::fusion::vector<A>)> func) {
                    return [f, func](boost::fusion::vector<A> vec) {
                        return f(func(vec));
                    };
                })), [](std::function<B(boost::fusion::vector<A>)> func) {
                    return func(boost::fusion::vector<A>());
                });
            },
            this->g);
    }

    LeftAdjoint<F, G, std::function<B(A)>> apply(LeftAdjoint<F, G, A> other) {
        return LeftAdjoint<F, G, std::function<B(A)>>(
            [this, other](boost::fusion::vector<std::function<std::function<B(A)>(boost::fusion::vector<A>)>> vec) {
                return boost::fusion::transform(this->f(boost::fusion::transform(vec, [other](std::function<std::function<B(A)>(boost::fusion::vector<A>)> func) {
                    return [func, other](boost::fusion::vector<A> vec) {
                        return func(other.f(vec));
                    };
                })), [](std::function<std::function<B(A)>(boost::fusion::vector<A>)> func) {
                    return func(boost::fusion::vector<A>());
                });
            },
            [this, other](boost::fusion::vector<G> vec) {
                return this->g(other.g(vec));
            });
    }

    template <typename B>
    LeftAdjoint<F, G, B> pure(B b) {
        return LeftAdjoint<F, G, B>(
            [b](boost::fusion::vector<std::function<B(boost::fusion::vector<A>)>> vec) {
                return boost::fusion::transform(vec, [b](std::function<B(boost::fusion::vector<A>)> func) {
                    return [b, func](boost::fusion::vector<A> vec) {
                        return b;
                    };
                });
            },
            [](boost::fusion::vector<G> vec) {
                return vec;
            });
    }
};

template <typename F, typename U>
struct Adjunction {
    std::function<U(F)> left_adjunct;
    std::function<F(U)> right_adjunct;

    Adjunction(std::function<U(F)> left_adjunct, std::function<F(U)> right_adjunct)
        : left_adjunct(left_adjunct), right_adjunct(right_adjunct) {}

    template <typename A>
    LeftAdjoint<F, std::vector<A>, A> adjoint_to_left_adjoint(U u) {
        return LeftAdjoint<F, std::vector<A>, A>(
            [this, u](boost::fusion::vector<std::function<A(boost::fusion::vector<A>)>> vec) {
                return boost::fusion::transform(vec, [this, u](std::function<A(boost::fusion::vector<A>)> func) {
                    return this->right_adjunct(u)(func);
                });
            },
            [](boost::fusion::vector<std::vector<A>> vec) {
                return vec;
            });
    }

    template <typename A>
    U left_adjoint_to_adjoint(LeftAdjoint<F, std::vector<A>, A> left_adjoint) {
        return this->left_adjunct(left_adjoint.f(boost::fusion::make_vector(std::function<A(boost::fusion::vector<A>)>([](boost::fusion::vector<A> vec) {
            return vec[0];
        }))));
    }
};