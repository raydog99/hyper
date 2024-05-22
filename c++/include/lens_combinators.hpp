#include <boost/optional.hpp>
#include <boost/variant.hpp>
#include <regex>
#include <string>
#include <vector>

using namespace std;

struct Str {
    string value;
};

struct Regex {
    regex re;
};

using RE = boost::variant<Str, Regex>;

struct Del {
    RE re;
    string value;
};

struct Store {
    RE re;
};

using Value = string;

using Counter = string;

using Seq = string;

struct Key {
    RE re;
};

using Label = string;

using Lens = boost::variant<Del, Store, Value, Counter, Seq, Key, Label>;

boost::optional<Lens> parser_return(Lens value) {
    return boost::optional<Lens>(value);
}

template <typename Pred>
boost::optional<Lens> parser_satisfy(Pred predicate, const string& input) {
    if (input.empty())
        return boost::none;

    Lens value = static_cast<Lens>(input[0]);
    if (predicate(value)) {
        return boost::optional<Lens>(value);
    } else {
        return boost::none;
    }
}

vector<string> concat(const Lens& l1, const Lens& l2, const vector<string>& input) {
    vector<string> result;
    for (const auto& s : input) {
        auto res1 = apply_lens(l1, s);
        for (const auto& r1 : res1) {
            auto res2 = apply_lens(l2, r1);
            result.insert(result.end(), res2.begin(), res2.end());
        }
    }
    return result;
}

string union_(const Lens& l1, const Lens& l2, const string& input) {
    auto res1 = apply_lens(l1, input);
    if (!res1.empty()) {
        return accumulate(res1.begin(), res1.end(), string());
    }
    return apply_lens(l2, input).front();
}

vector<string> repetition(const string& op, const Lens& l, const vector<string>& input) {
    vector<string> result;
    for (const auto& s : input) {
        auto res = apply_lens(l, s);
        result.insert(result.end(), res.begin(), res.end());
        if (op == "*") {
            continue;
        } else if (op == "+" && res.empty()) {
            return {};
        } else if (op == "?") {
            break;
        }
    }
    return result;
}

vector<vector<string>> subtree(const Lens& l, const vector<string>& input) {
    vector<vector<string>> result;
    for (const auto& s : input) {
        auto res = apply_lens(l, s);
        result.push_back(res);
    }
    return result;
}

string square(const Lens& left, const Lens& body, const Lens& right, const string& input) {
    auto res1 = apply_lens(left, input);
    if (res1.empty()) {
        return "";
    }
    auto res2 = apply_lens(right, res1[0]);
    if (res2.empty()) {
        return "";
    }
    auto mid = res2[0].substr(res1[0].length());
    auto mid_res = apply_lens(body, mid);
    string result = res1[0] + accumulate(mid_res.begin(), mid_res.end(), string()) +
                    res2[0].substr(mid.length());
    return result;
}