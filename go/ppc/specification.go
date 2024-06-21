package capitan

import (
	"regexp"
	"strings"
)

type Rule struct{}

type T struct {
	MatchTemplate   string
	Rule            *Rule
	RewriteTemplate *string
}

func Create(rewriteTemplate *string, rule *Rule, matchTemplate string) T {
	return T{
		MatchTemplate:   matchTemplate,
		Rule:            rule,
		RewriteTemplate: rewriteTemplate,
	}
}

type Extracted interface {
	toRegex() string
}

type Regex string
type ContiguousWhitespace string
type NonSpace string

func (r Regex) toRegex() string                 { return string(r) }
func (c ContiguousWhitespace) toRegex() string  { return "\\s+" }
func (n NonSpace) toRegex() string              { return regexp.QuoteMeta(string(n)) }

func escape(s string) string {
	return regexp.QuoteMeta(s)
}

func ToRegex(t T) string {
	extracted := extract(t.MatchTemplate)
	parts := make([]string, len(extracted))
	for i, e := range extracted {
		parts[i] = e.toRegex()
	}
	return "(" + strings.Join(parts, "") + ")"
}

func extract(template string) []Extracted {
	return nil
}