#include <boost/mpl/vector.hpp>
#include <boost/mpl/pair.hpp>

namespace mpl = boost::mpl;

template <typename Category>
struct MonoidalCategory {
    typedef typename Category::Obj Obj;
    typedef typename Category::Mor Mor;

    static Mor id(Obj obj) {
        return Category::id(obj);
    }

    static Mor compose(Mor f, Mor g) {
        return Category::compose(f, g);
    }

    typedef mpl::vector<Obj, Obj> TensorObjs;
    typedef mpl::pair<Mor, Mor> TensorMors;

    static TensorObjs tensor(Obj a, Obj b) {
        return mpl::vector<Obj, Obj>(a, b);
    }

    static TensorMors tensorMor(Mor f, Mor g) {
        return mpl::pair<Mor, Mor>(f, g);
    }
};