#include <boost/function.hpp>
#include <iostream>
#include <string>
#include <vector>

enum class TermType { Var, App, Lam };

struct Term {
  TermType type;
  boost::any value;

  template <typename T>
  T eval(const std::vector<std::pair<std::string, T>>& env) const {
    switch (type) {
      case TermType::Var:
        return boost::any_cast<T>(env[std::any_cast<int>(value)])[1];
      case TermType::App: {
        auto f = boost::any_cast<boost::function<T(T)>>((*this).value);
        auto arg = std::any_cast<Term>(*value.embedded_type().second).eval<T>(env);
        return f(arg);
      }
      case TermType::Lam: {
        auto var = std::any_cast<std::string>(value);
        auto body = std::any_cast<Term>(*value.embedded_type().second);
        return [var, body](T arg) {
          std::vector<std::pair<std::string, T>> new_env(env);
          new_env.push_back({var, arg});
          return body.eval<T>(new_env);
        };
      }
    }
  }
};

Term fix(boost::function<Term(Term)> f) {
  return Term{TermType::Lam,
              {"x", Term{TermType::App, {boost::any(f), Term{TermType::Var, 0}}}}};
}

boost::function<boost::any(boost::any)> id() {
  return [](boost::any x) { return x; };
}

boost::function<boost::function<boost::any(boost::any)>> k() {
  return [](boost::any x) { return [x](boost::any y) { return x; }; };
}

boost::function<boost::function<boost::function<boost::any(boost::any)>(boost::any)>(boost::any)>> b() {
  return [](boost::any x) {
    return [x](boost::any y) { return [y](boost::any z) { return y; }; };
  };
}
