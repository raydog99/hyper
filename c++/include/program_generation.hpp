#include <iostream>
#include <string>
#include <unordered_map>
#include <variant>
#include <functional>

using Ident = std::string;

struct Var { Ident x; };
struct Lam { Ident x; std::variant<Var, Lam, std::pair<std::variant<void*>, std::variant<void*>>> e; };
struct App { std::variant<void*> f, a; };
struct LamD { Ident x; std::variant<Var, Lam, std::pair<std::variant<void*>, std::variant<void*>>> e; };
struct AppD { std::variant<void*> f, a; };

using Exp = std::variant<Var, Lam, App, LamD, AppD>;
using Value = std::variant<std::function<Value(Value)>, Exp>;