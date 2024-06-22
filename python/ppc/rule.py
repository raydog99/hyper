from dataclasses import dataclass
from typing import List, Tuple, Union, Optional

@dataclass
class String:
    value: str

@dataclass
class Template:
    templates: List[str]

@dataclass
class Equal:
    left: 'Ast'
    right: 'Ast'

@dataclass
class NotEqual:
    left: 'Ast'
    right: 'Ast'

@dataclass
class Rewrite:
    source: 'Ast'
    replacement: Tuple['Ast', 'Ast']

@dataclass
class Match:
    source: 'Ast'
    patterns: List[Tuple['Ast', List['Ast']]]

class True_:
    pass

class False_:
    pass

@dataclass
class Option:
    name: str

Ast = Union[String, Template, Equal, NotEqual, Rewrite, Match, True_, False_, Option]
Rule = List[Ast]

@dataclass
class Options:
    nested: bool = False
    strict: bool = False

class ParseError(Exception):
    pass

def parse_ast(tokens: List[str]) -> Tuple[Ast, List[str]]:
    if not tokens:
        raise ParseError("Unexpected end of input")

    if tokens[0] == "true":
        return True_(), tokens[1:]
    elif tokens[0] == "false":
        return False_(), tokens[1:]
    elif tokens[0] == "option":
        if len(tokens) < 2:
            raise ParseError("Option name expected")
        return Option(tokens[1]), tokens[2:]
    elif tokens[0] == "=":
        if len(tokens) < 4:
            raise ParseError("Invalid equal expression")
        lhs, _ = parse_ast(tokens[1:2])
        rhs, rest = parse_ast(tokens[3:])
        return Equal(lhs, rhs), rest
    elif tokens[0] == "!=":
        if len(tokens) < 3:
            raise ParseError("Invalid not equal expression")
        lhs, _ = parse_ast(tokens[1:2])
        rhs, rest = parse_ast(tokens[2:])
        return NotEqual(lhs, rhs), rest
    elif tokens[0] == "->":
        if len(tokens) < 4:
            raise ParseError("Invalid rewrite expression")
        lhs, _ = parse_ast(tokens[1:2])
        rhs, rest = parse_ast(tokens[3:])
        return Rewrite(lhs, (rhs, rhs)), rest
    else:
        return String(tokens[0]), tokens[1:]

def parse_rule(tokens: List[str]) -> Rule:
    rule = []
    while tokens:
        if tokens[0] == ",":
            tokens = tokens[1:]
            continue
        ast, tokens = parse_ast(tokens)
        rule.append(ast)
    return rule

def create(rule_text: str) -> Optional[Rule]:
    tokens = rule_text.split()
    if not tokens or tokens[0] != "where":
        return None
    try:
        return parse_rule(tokens[1:])
    except ParseError:
        return None

def options(rule: Rule) -> Options:
    opts = Options()
    for ast in rule:
        if isinstance(ast, Option):
            if ast.name == "nested":
                opts.nested = True
            elif ast.name == "strict":
                opts.strict = True
    return opts

def is_strict(rule: Rule) -> bool:
    return options(rule).strict
