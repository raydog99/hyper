#include <boost/spirit/include/qi.hpp>
#include <iostream>
#include <string>
#include <vector>

namespace qi = boost::spirit::qi;

struct Hole {
  std::string pattern;
  std::string variable;
  int offset;
  std::string kind;
};

struct Constant {
  std::string value;
};

typedef std::vector<std::variant<Hole, Constant>> Matches;

template <typename Iterator>
struct HoleParser : qi::grammar<Iterator, Hole(), qi::space_type> {
  HoleParser() : HoleParser::base_type(start) {
    using qi::char_;
    using qi::lit;
    using qi::lexeme;
    using qi::no_case;
    using qi::alpha;
    using qi::alnum;

    identifier = lexeme[alpha >> *(alnum | '_' | '\'')];
    holePattern = (lit("") | lit("[") | lit("{") | lit("(") | lit("<")) >>
                  identifier >> (lit("") | lit("]") | lit("}") | lit(")") | lit(">"));
    regexHole = lit("$") >> identifier >> lit("~") >> +~char_('$') >> lit("$");
    start = holePattern[qi::_val = phx::construct<Hole>(qi::_1, qi::_2, 0, "value")] |
            regexHole[qi::_val = phx::construct<Hole>(qi::_1, qi::_2, 0, "value")];
  }

  qi::rule<Iterator, std::string(), qi::space_type> identifier;
  qi::rule<Iterator, Hole(), qi::space_type> holePattern, regexHole, start;
};

template <typename Iterator>
struct ConstantParser : qi::grammar<Iterator, Constant(), qi::space_type> {
  ConstantParser() : ConstantParser::base_type(start) {
    using qi::lit;
    using qi::no_case;

    start = +(~lit('$'));
  }

  qi::rule<Iterator, Constant(), qi::space_type> start;
};

template <typename Iterator>
struct TemplateParser : qi::grammar<Iterator, Matches(), qi::space_type> {
  TemplateParser() : TemplateParser::base_type(start) {
    start = *(holeParser | constantParser);
  }

  HoleParser<Iterator> holeParser;
  ConstantParser<Iterator> constantParser;
  qi::rule<Iterator, Matches(), qi::space_type> start;
};

Matches parseTemplate(const std::string& input) {
  std::string::const_iterator begin = input.begin();
  std::string::const_iterator end = input.end();
  Matches matches;
  TemplateParser<std::string::const_iterator> parser;
  bool success = qi::phrase_parse(begin, end, parser, qi::space, matches);
  if (!success) {
    std::cerr << "Failed to parse template" << std::endl;
  }
  return matches;
}