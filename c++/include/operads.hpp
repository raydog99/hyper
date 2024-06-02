#include <boost/optional.hpp>
#include <vector>
#include <functional>
#include <unordered_map>

template <typename T>
using Seq = std::vector<T>;

template <typename T>
struct Operad {
    std::vector<T> colors;
    std::unordered_map<std::pair<Seq<T>, T>, int> operations;
    std::function<boost::optional<int>(std::pair<Seq<T>, int>, Seq<int>)> composition;
    std::function<int(T)> identity;
};

template <typename T>
Operad<T> makeOperad(
    std::vector<T> colors,
    std::vector<std::tuple<Seq<T>, T, int>> operations,
    std::function<boost::optional<int>(std::pair<Seq<T>, int>, Seq<int>)> composition,
    std::function<int(T)> identity
) {
    std::unordered_map<std::pair<Seq<T>, T>, int> ops;
    int i = 0;
    for (auto& [profile, color, _] : operations) {
        ops[std::make_pair(profile, color)] = i++;
    }
    return {colors, ops, composition, identity};
}

template <typename T>
boost::optional<int> findOperation(const Operad<T>& operad, const Seq<T>& profile, const T& color) {
    auto it = operad.operations.find(std::make_pair(profile, color));
    if (it == operad.operations.end()) {
        return boost::none;
    }
    return it->second;
}

template <typename T>
boost::optional<int> compose(const Operad<T>& operad, int opIndex, const Seq<int>& operands) {
    return operad.composition(std::make_pair(Seq<T>(), opIndex), operands);
}

template <typename T>
int identityOp(const Operad<T>& operad, const T& color) {
    return operad.identity(color);
}