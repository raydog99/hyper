#include <boost/variant.hpp>

struct ElementVisitor;
struct ContainerVisitor;

typedef boost::variant<ElementVisitor, ContainerVisitor> Visitor;

struct Element {
    int weight;
};

struct Container {
    int capacity;
};

struct IndependComp {
    Visitor vis1, vis2;

    IndependComp(const Visitor& v1, const Visitor& v2) : vis1(v1), vis2(v2) {}

    template <typename T>
    IndependComp before(const T& o) const {
        return IndependComp(boost::apply_visitor(boost::static_visitor<Visitor>([&](auto&& v) { return v.before(o); }), vis1),
                             boost::apply_visitor(boost::static_visitor<Visitor>([&](auto&& v) { return v.before(o); }), vis2));
    }

    template <typename T>
    IndependComp after(const T& o) const {
        return IndependComp(boost::apply_visitor(boost::static_visitor<Visitor>([&](auto&& v) { return v.after(o, vis1); }), vis1),
                             boost::apply_visitor(boost::static_visitor<Visitor>([&](auto&& v) { return v.after(o, vis2); }), vis2));
    }
};

struct ThreadedComp {
    Visitor vis1, vis2;

    ThreadedComp(const Visitor& v1, const Visitor& v2) : vis1(v1), vis2(v2) {}

    template <typename T>
    ThreadedComp before(const T& o) const {
        auto v1 = boost::apply_visitor(boost::static_visitor<Visitor>([&](auto&& v) { return v.before(o); }), vis1);
        auto v2 = boost::apply_visitor(boost::static_visitor<Visitor>([&](auto&& v) { return v.before(o, boost::get<CompReceiver>(vis1)); }), vis2);
        return ThreadedComp(v1, v2);
    }

    template <typename T>
    ThreadedComp after(const T& o) const {
        auto v1 = boost::apply_visitor(boost::static_visitor<Visitor>([&](auto&& v) { return v.after(o, vis1); }), vis1);
        auto v2 = boost::apply_visitor(boost::static_visitor<Visitor>([&](auto&& v) { return v.after(o, vis2); }), vis2);
        return ThreadedComp(v1, v2);
    }

    Visitor exportVisitor() const {
        return boost::apply_visitor(boost::static_visitor<Visitor>([](auto&& v) { return v.exportVisitor(); }), vis2);
    }
};

struct ConditionalComp {
    Visitor vis1, vis2;

    ConditionalComp(const Visitor& v1, const Visitor& v2) : vis1(v1), vis2(v2) {}

    template <typename T>
    ConditionalComp before(const T& o) const {
        auto v1 = boost::apply_visitor(boost::static_visitor<Visitor>([&](auto&& v) { return v.before(o); }), vis1);
        if (boost::apply_visitor(boost::static_visitor<bool>([](auto&& v) { return v.continueVisit(); }), v1)) {
            auto v2 = boost::apply_visitor(boost::static_visitor<Visitor>([&](auto&& v) { return v.before(o); }), vis2);
            return ConditionalComp(v1, v2);
        } else {
            return ConditionalComp(v1, vis2);
        }
    }

    template <typename T>
    ConditionalComp after(const T& o) const {
        auto v1 = boost::apply_visitor(boost::static_visitor<Visitor>([&](auto&& v) { return v.after(o, vis1); }), vis1);
        if (boost::apply_visitor(boost::static_visitor<bool>([](auto&& v) { return v.continueVisit(); }), v1)) {
            auto v2 = boost::apply_visitor(boost::static_visitor<Visitor>([&](auto&& v) { return v.after(o, vis2); }), vis2);
            return ConditionalComp(v1, v2);
        } else {
            return ConditionalComp(v1, vis2);
        }
    }
};