#include <boost/algorithm/string.hpp>
#include <boost/regex.hpp>
#include <iostream>
#include <map>
#include <string>
#include <vector>

using Variable = std::string;
using Pattern = std::string;
using Offset = size_t;

enum class AttributeKind {
    Value,
    Length,
    Lines,
    OffsetStart,
    OffsetEnd,
    LineStart,
    LineEnd,
    ColumnStart,
    ColumnEnd,
    FilePath,
    FileName,
    FileDirectory,
    Lowercase,
    Uppercase,
    Capitalize,
    Uncapitalize,
    UpperCamelCase,
    LowerCamelCase,
    UpperSnakeCase,
    LowerSnakeCase,
    External
};

struct Hole {
    Pattern pattern;
    Variable variable;
    Offset offset;
    AttributeKind kind;
};

struct TemplateElement {
    std::string constant;
    std::optional<Hole> hole;
};

AttributeKind attributeToKind(const std::string& s) {
    if (s == "value")
        return AttributeKind::Value;
    else if (s == "length")
        return AttributeKind::Length;
    else if (s == "lines")
        return AttributeKind::Lines;
    else if (s == "offset" || s == "offset.start")
        return AttributeKind::OffsetStart;
    else if (s == "offset.end")
        return AttributeKind::OffsetEnd;
    else if (s == "line" || s == "line.start")
        return AttributeKind::LineStart;
    else if (s == "line.end")
        return AttributeKind::LineEnd;
    else if (s == "column" || s == "column.start")
        return AttributeKind::ColumnStart;
    else if (s == "column.end")
        return AttributeKind::ColumnEnd;
    else if (s == "file" || s == "file.path")
        return AttributeKind::FilePath;
    else if (s == "file.name")
        return AttributeKind::FileName;
    else if (s == "file.directory")
        return AttributeKind::FileDirectory;
    else if (s == "lowercase")
        return AttributeKind::Lowercase;
    else if (s == "UPPERCASE")
        return AttributeKind::Uppercase;
    else if (s == "Capitalize")
        return AttributeKind::Capitalize;
    else if (s == "uncapitalize")
        return AttributeKind::Uncapitalize;
    else if (s == "UpperCamelCase")
        return AttributeKind::UpperCamelCase;
    else if (s == "lowerCamelCase")
        return AttributeKind::LowerCamelCase;
    else if (s == "UPPER_SNAKE_CASE")
        return AttributeKind::UpperSnakeCase;
    else if (s == "lower_snake_case")
        return AttributeKind::LowerSnakeCase;
    else if (s == "lsif.hover")
        return AttributeKind::External;
    else
        throw std::runtime_error("invalid attribute " + s);
}

std::string camelToSnake(const std::string& s) {
    std::vector<std::string> words;
    std::string word;
    for (char c : s) {
        if (std::isupper(c)) {
            if (!word.empty()) {
                words.push_back(boost::to_lower_copy(word));
                word.clear();
            }
            word.push_back(std::tolower(c));
        } else {
            word.push_back(c);
        }
    }
    if (!word.empty()) {
        words.push_back(boost::to_lower_copy(word));
    }
    return boost::join(words, "_");
}

std::optional<std::string> substituteKind(const std::optional<std::string>& filepath,
                                          const TemplateElement& element,
                                          const std::map<Variable, std::string>& env) {
    if (element.constant.empty()) {
        const auto& hole = element.hole.value();
        auto lookup = [&](const Variable& var) -> std::optional<std::string> {
            auto it = env.find(var);
            if (it == env.end()) {
                return std::nullopt;
            } else {
                return it->second;
            }
        };

        auto matchStart = [&](const Variable& var) -> std::optional<std::string> {
            return lookup(var).has_value() ? std::make_optional("0") : std::nullopt; // Placeholder implementation
        };

        auto matchEnd = [&](const Variable& var) -> std::optional<std::string> {
            auto value = lookup(var);
            if (value.has_value()) {
                return std::make_optional(std::to_string(value->length()));
            } else {
                return std::nullopt;
            }
        };

        auto lineStart = [&](const std::optional<std::string>& filepath, const Variable& var) -> std::optional<std::string> {
            return matchStart(var).has_value() ? std::make_optional("1") : std::nullopt; // Placeholder implementation
        };

        auto lineEnd = [&](const std::optional<std::string>& filepath, const Variable& var) -> std::optional<std::string> {
            return matchEnd(var).has_value() ? std::make_optional("1") : std::nullopt; // Placeholder implementation
        };

        auto columnStart = [&](const std::optional<std::string>& filepath, const Variable& var) -> std::optional<std::string> {
            return matchStart(var).has_value() ? std::make_optional("1") : std::nullopt; // Placeholder implementation
        };

        auto columnEnd = [&](const std::optional<std::string>& filepath, const Variable& var) -> std::optional<std::string> {
            return matchEnd(var).has_value() ? std::make_optional("1") : std::nullopt; // Placeholder implementation
        };

        auto offsetToLineColumn = [](const std::string& source, Offset offset) -> std::pair<size_t, size_t> {
            return std::make_pair(1, 1); // Placeholder implementation
        };

        auto stripVar = [](const Variable& var) -> std::optional<Variable> {
            static const boost::regex re(R"(\[(.*?)\])");
            boost::smatch match;
            if (boost::regex_search(var, match, re)) {
                if (match.size() == 2) {
                    return std::make_optional(match[1]);
                }
            }
            return std::nullopt;
        };

        switch (hole.kind) {
            case AttributeKind::Value:
                return lookup(hole.variable);
            case AttributeKind::Length:
                return lookup(hole.variable).has_value()
                       ? std::make_optional(std::to_string(lookup(hole.variable)->length()))
                       : std::nullopt;
            case AttributeKind::Lines:
                return lookup(hole.variable).has_value()
                       ? std::make_optional(std::to_string(std::count(lookup(hole.variable)->begin(), lookup(hole.variable)->end(), '\n') + 1))
                       : std::nullopt;
            case AttributeKind::OffsetStart:
                return matchStart(hole.variable);
            case AttributeKind::OffsetEnd:
                return matchEnd(hole.variable);
            case AttributeKind::LineStart:
                return lineStart(filepath, hole.variable);
            case AttributeKind::LineEnd:
                return lineEnd(filepath, hole.variable);
            case AttributeKind::ColumnStart:
                return columnStart(filepath, hole.variable);
            }
    }
}