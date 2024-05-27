#include <boost/variant.hpp>
#include <boost/recursive_variant.hpp>

// Booleans
using Bool = boost::variant<bool>;

// Natural Numbers
struct zero {}; // zero constructor
using Nat = boost::variant<zero, boost::recursive_variant_>;

// Finite Lists
template <typename T>
using FiniteList = boost::variant<boost::recursive_variant_, std::vector<T>>;

// Finite Binary Trees
template <typename T>
struct leaf {};
template <typename T>
struct bnode {
    T value;
    std::pair<boost::recursive_variant_, boost::recursive_variant_> children;
};
template <typename T>
using BTree = boost::variant<leaf<T>, bnode<T>>;

// Infinite Binary Trees
template <typename T>
using InfBTree = boost::recursive_variant_<
    T,
    std::pair<boost::recursive_variant_, boost::recursive_variant_>
>;

// Infinite Lists
template <typename T>
using InfList = boost::recursive_variant_<
    T,
    boost::recursive_variant_
>;

// Non-empty Trees
template <typename T>
using NETree = boost::variant<BTree<T>, InfBTree<T>>;

// Non-empty Lists
template <typename T>
using NEList = boost::variant<FiniteList<T>, InfList<T>>;

// CoLists
template <typename T>
using CoList = boost::variant<
    std::pair<T, boost::recursive_variant_>
>;

// Co-Non-empty Trees
template <typename T>
using CoNETree = boost::recursive_variant_<
    T,
    std::pair<boost::recursive_variant_, boost::recursive_variant_>
>;

// Co-Non-empty Lists
template <typename T>
using CoNEList = boost::recursive_variant_<
    T,
    boost::recursive_variant_
>;