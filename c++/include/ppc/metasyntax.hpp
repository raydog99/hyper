#include <boost/variant.hpp>
#include <string>
#include <vector>
#include <unordered_set>

struct HoleType {
    enum Value {
        Everything,
        Expression,
        Alphanum,
        NonSpace,
        Line,
        Blank
    };
};

typedef std::pair<std::optional<std::string>, std::optional<std::string>> Delimited;
typedef std::unordered_set<std::string> ReservedIdentifiers;

typedef boost::variant<Delimited, ReservedIdentifiers> Delimiter;

struct Hole {
    HoleType::Value type;
    Delimiter delimiter;
};

struct Regex {
    std::string start;
    char sep;
    std::string end;
};

typedef std::vector<boost::variant<Hole, Regex>> Syntax;

struct Alias {
    std::string pattern;
    std::string matchTemplate;
    std::optional<std::string> rule;
};

struct Metasyntax {
    Syntax syntax;
    std::string identifier;
    std::vector<Alias> aliases;
};

Syntax defaultSyntax() {
    Syntax syntax;
    syntax.emplace_back(Regex{":[", '~', "]"});
    syntax.emplace_back(Hole{HoleType::Everything, Delimited{std::optional{":["},std::optional{"]"}}});
    syntax.emplace_back(Hole{HoleType::Expression, Delimited{std::optional{":["},std::optional{":e]"}}});
    syntax.emplace_back(Hole{HoleType::Alphanum, Delimited{std::optional{":[["},std::optional{"]]"}}});
    syntax.emplace_back(Hole{HoleType::NonSpace, Delimited{std::optional{":["},std::optional{".]"}}});
    syntax.emplace_back(Hole{HoleType::Line, Delimited{std::optional{":["},std::optional{"\\n]"}}});
    syntax.emplace_back(Hole{HoleType::Blank, Delimited{std::optional{":[ "},std::optional{"]"}}});
    syntax.emplace_back(Hole{HoleType::Expression, ReservedIdentifiers{"α", "β", "γ", "δ", "ε", "ζ", "η", "θ", "ι", "κ", "λ", "μ", "ξ", "π", "ρ", "ς", "σ", "τ", "υ", "φ", "χ", "ψ", "ω"}});
    syntax.emplace_back(Hole{HoleType::Everything, ReservedIdentifiers{"Γ", "Δ", "Θ", "Λ", "Ξ", "Π", "Σ", "Φ", "Ψ", "Ω"}});
    return syntax;
}

std::string defaultIdentifier() {
    return "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_";
}

std::vector<Alias> defaultAliases() {
    std::vector<Alias> aliases;
    aliases.emplace_back(Alias{"...", ":[_]", std::nullopt});
    return aliases;
}

Metasyntax defaultMetasyntax() {
    return Metasyntax{defaultSyntax(), defaultIdentifier(), defaultAliases()};
}

Metasyntax create(const Syntax& syntax, const std::string& identifier, const std::vector<Alias>& aliases) {
    return Metasyntax{syntax, identifier, aliases};
}

Metasyntax Default() {
    return create(defaultSyntax(), defaultIdentifier(), defaultAliases());
}