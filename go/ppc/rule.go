package capitan

import (
    "errors"
    "strings"
)

type Ast interface{}

type StringAst struct {
    Value string
}

type TemplateAst struct {
    Templates []string
}

type EqualAst struct {
    Left, Right Ast
}

type NotEqualAst struct {
    Left, Right Ast
}

type RewriteAst struct {
    Source      Ast
    Replacement [2]Ast
}

type MatchAst struct {
    Source   Ast
    Patterns [][2][]Ast
}

type TrueAst struct{}
type FalseAst struct{}

type OptionAst struct {
    Name string
}

type Rule []Ast

type Options struct {
    Nested bool
    Strict bool
}

func parseAst(tokens []string) (Ast, []string, error) {
    if len(tokens) == 0 {
        return nil, nil, errors.New("Unexpected end of input")
    }

    switch tokens[0] {
    case "true":
        return TrueAst{}, tokens[1:], nil
    case "false":
        return FalseAst{}, tokens[1:], nil
    case "option":
        if len(tokens) < 2 {
            return nil, nil, errors.New("Option name expected")
        }
        return OptionAst{Name: tokens[1]}, tokens[2:], nil
    case "=":
        if len(tokens) < 4 {
            return nil, nil, errors.New("Invalid equal expression")
        }
        lhs, _, _ := parseAst(tokens[1:2])
        rhs, rest, _ := parseAst(tokens[3:])
        return EqualAst{Left: lhs, Right: rhs}, rest, nil
    case "!=":
        if len(tokens) < 3 {
            return nil, nil, errors.New("Invalid not equal expression")
        }
        lhs, _, _ := parseAst(tokens[1:2])
        rhs, rest, _ := parseAst(tokens[2:])
        return NotEqualAst{Left: lhs, Right: rhs}, rest, nil
    case "->":
        if len(tokens) < 4 {
            return nil, nil, errors.New("Invalid rewrite expression")
        }
        lhs, _, _ := parseAst(tokens[1:2])
        rhs, rest, _ := parseAst(tokens[3:])
        return RewriteAst{Source: lhs, Replacement: [2]Ast{rhs, rhs}}, rest, nil
    default:
        return StringAst{Value: tokens[0]}, tokens[1:], nil
    }
}

func parseRule(tokens []string) (Rule, error) {
    var rule Rule
    for len(tokens) > 0 {
        if tokens[0] == "," {
            tokens = tokens[1:]
            continue
        }
        ast, rest, err := parseAst(tokens)
        if err != nil {
            return nil, err
        }
        rule = append(rule, ast)
        tokens = rest
    }
    return rule, nil
}

func Create(ruleText string) (Rule, error) {
    tokens := strings.Fields(ruleText)
    if len(tokens) == 0 || tokens[0] != "where" {
        return nil, errors.New("Rule must start with 'where'")
    }
    return parseRule(tokens[1:])
}

func (r Rule) Options() Options {
    opts := Options{false, false}
    for _, ast := range r {
        if option, ok := ast.(OptionAst); ok {
            switch option.Name {
            case "nested":
                opts.Nested = true
            case "strict":
                opts.Strict = true
            }
        }
    }
    return opts
}

func (r Rule) IsStrict() bool {
    return r.Options().Strict
}