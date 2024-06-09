#include <boost/function.hpp>
#include <boost/utility.hpp>

template <typename G, typename H, typename A>
struct Rift {
    boost::function<boost::any(boost::function<std::pair<A, boost::any>(boost::any)>, boost::any)> runRift;

    template <typename B>
    Rift<G, H, B> contramap(boost::function<B(A)> f) const {
        return Rift<G, H, B>{[this, f](boost::function<std::pair<B, boost::any>(boost::any)> bac, boost::any gc) {
            return this->runRift([bac, f](boost::any b) {
                auto pair = bac(b);
                return std::make_pair(f(boost::any_cast<A>(pair.first)), pair.second);
            }, gc);
        }};
    }
};

template <typename G, typename A, typename B, typename C>
Rift<G, G, C> divide(boost::function<B(A)> f, const Rift<G, G, A>& g, const Rift<G, G, B>& h) {
    return Rift<G, G, C>{[f, g, h](boost::function<std::pair<C, boost::any>(boost::any)> bac, boost::any gc) {
        return g.runRift([bac, f](boost::any a) {
            auto pair = bac(a);
            return std::make_pair(f(boost::any_cast<A>(pair.first)), pair.second);
        }, gc);
    }};
}