// File: rule.hpp
#pragma once

#include <boost/variant.hpp>
#include <boost/optional.hpp>
#include <string>
#include <vector>

struct String;
struct Template;
struct Equal;
struct NotEqual;
struct Rewrite;
struct Match;
struct True;
struct False;
struct Option;

using Ast = boost::variant
    String,
    Template,
    boost::recursive_wrapper<Equal>,
    boost::recursive_wrapper<NotEqual>,
    boost::recursive_wrapper<Rewrite>,
    boost::recursive_wrapper<Match>,
    True,
    False,
    Option
>;

struct String { std::string value; };
struct Template { std::vector<std::string> templates; };
struct Equal { Ast left; Ast right; };
struct NotEqual { Ast left; Ast right; };
struct Rewrite { Ast source; std::pair<Ast, Ast> replacement; };
struct Match { Ast source; std::vector<std::pair<Ast, std::vector<Ast>>> patterns; };
struct True {};
struct False {};
struct Option { std::string name; };

using Rule = std::vector<Ast>;

struct Options {
    bool nested = false;
    bool strict = false;
};

boost::optional<Rule> create(const std::string& rule_text);
Options options(const Rule& rule);
bool is_strict(const Rule& rule);

} // namespace rule

// File: rule.cpp
#include "rule.hpp"
#include <boost/algorithm/string.hpp>
#include <stdexcept>

namespace rule {

class parse_error : public std::runtime_error {
public:
    parse_error(const std::string& msg) : std::runtime_error(msg) {}
};

std::pair<Ast, std::vector<std::string>::const_iterator>
parse_ast(std::vector<std::string>::const_iterator begin, std::vector<std::string>::const_iterator end) {
    if (begin == end) {
        throw parse_error("Unexpected end of input");
    }

    if (*begin == "true") {
        return {True{}, std::next(begin)};
    } else if (*begin == "false") {
        return {False{}, std::next(begin)};
    } else if (*begin == "option") {
        if (std::next(begin) == end) {
            throw parse_error("Option name expected");
        }
        return {Option{*std::next(begin)}, std::next(begin, 2)};
    } else if (*begin == "=") {
        if (std::distance(begin, end) < 4) {
            throw parse_error("Invalid equal expression");
        }
        auto [lhs, _] = parse_ast(std::next(begin), end);
        auto [rhs, rest] = parse_ast(std::next(begin, 3), end);
        return {Equal{lhs, rhs}, rest};
    } else if (*begin == "!=") {
        if (std::distance(begin, end) < 3) {
            throw parse_error("Invalid not equal expression");
        }
        auto [lhs, _] = parse_ast(std::next(begin), end);
        auto [rhs, rest] = parse_ast(std::next(begin, 2), end);
        return {NotEqual{lhs, rhs}, rest};
    } else if (*begin == "->") {
        if (std::distance(begin, end) < 4) {
            throw parse_error("Invalid rewrite expression");
        }
        auto [lhs, _] = parse_ast(std::next(begin), end);
        auto [rhs, rest] = parse_ast(std::next(begin, 3), end);
        return {Rewrite{lhs, {rhs, rhs}}, rest};
    } else {
        return {String{*begin}, std::next(begin)};
    }
}

Rule parse_rule(const std::vector<std::string>& tokens) {
    Rule rule;
    auto it = tokens.begin();
    while (it != tokens.end()) {
        if (*it == ",") {
            ++it;
            continue;
        }
        auto [ast, next] = parse_ast(it, tokens.end());
        rule.push_back(ast);
        it = next;
    }
    return rule;
}

boost::optional<Rule> create(const std::string& rule_text) {
    std::vector<std::string> tokens;
    boost::split(tokens, rule_text, boost::is_any_of(" \t\n"), boost::token_compress_on);

    if (tokens.empty() || tokens[0] != "where") {
        return boost::none;
    }

    try {
        return parse_rule(std::vector<std::string>(tokens.begin() + 1, tokens.end()));
    } catch (const parse_error& e) {
        return boost::none;
    }
}

Options options(const Rule& rule) {
    Options opts;
    for (const auto& ast : rule) {
        if (const Option* opt = boost::get<Option>(&ast)) {
            if (opt->name == "nested") opts.nested = true;
            else if (opt->name == "strict") opts.strict = true;
        }
    }
    return opts;
}

bool is_strict(const Rule& rule) {
    return options(rule).strict;
}