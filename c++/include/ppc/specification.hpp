#include <boost/optional.hpp>
#include <boost/regex.hpp>
#include <string>
#include <vector>

namespace capitan {

struct Rule {};

struct T {
    std::string match_template;
    boost::optional<Rule> rule;
    boost::optional<std::string> rewrite_template;
};

T create(boost::optional<std::string> rewrite_template, boost::optional<Rule> rule, std::string match_template) {
    return {std::move(match_template), std::move(rule), std::move(rewrite_template)};
}

class Extracted {
public:
    virtual ~Extracted() = default;
    virtual std::string to_regex() const = 0;
};

class Regex : public Extracted {
    std::string content;
public:
    explicit Regex(std::string s) : content(std::move(s)) {}
    std::string to_regex() const override { return content; }
};

class ContiguousWhitespace : public Extracted {
public:
    std::string to_regex() const override { return "\\s+"; }
};

class NonSpace : public Extracted {
    std::string content;
public:
    explicit NonSpace(std::string s) : content(std::move(s)) {}
    std::string to_regex() const override { return boost::regex_replace(content, boost::regex("[.\\\\+*?\\[\\^\\]$(){}=!<>|:\\-]"), "\\$&"); }
};

std::vector<std::unique_ptr<Extracted>> extract(const std::string& template_str) {
    return {};
}

std::string to_regex(const T& t) {
    auto extracted = extract(t.match_template);
    std::string result = "(";
    for (const auto& e : extracted) {
        result += e->to_regex();
    }
    result += ")";
    return result;
}

}