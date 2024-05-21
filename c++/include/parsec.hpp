#include <boost/optional.hpp>

template <typename T>
boost::optional<T> parser_return(T value)
{
    return boost::optional<T>(value);
}

template <typename T, typename Pred>
boost::optional<T> parser_satisfy(Pred predicate, std::string input)
{
    if (input.empty())
        return boost::none;
    
    T value = static_cast<T>(input[0]);
    if (predicate(value)) {
        return boost::optional<T>(value);
    } else {
        return boost::none;
    }
}