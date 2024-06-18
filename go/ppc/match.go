package capitan

import (
	"fmt"
	"regexp"
	"strings"
)

type Hole struct {
	Pattern  string
	Variable string
	Offset   int
	Kind     string
}

func (h Hole) String() string {
	return fmt.Sprintf("Hole{Pattern: %q, Variable: %q, Offset: %d, Kind: %q}", h.Pattern, h.Variable, h.Offset, h.Kind)
}

type Constant struct {
	Value string
}

func (c Constant) String() string {
	return fmt.Sprintf("Constant{Value: %q}", c.Value)
}

type Match interface {
	Hole | Constant
}

func ParseTemplate(input string) ([]Match, error) {
	var matches []Match
	offset := 0

	for len(input) > 0 {
		hole, rest, err := parseHole(input)
		if err == nil {
			hole.Offset = offset
			matches = append(matches, hole)
			input = rest
			offset += len(hole.Pattern)
			continue
		}

		constant, rest, err := parseConstant(input)
		if err == nil {
			matches = append(matches, constant)
			input = rest
			offset += len(constant.Value)
			continue
		}

		return nil, fmt.Errorf("failed to parse template at offset %d: %q", offset, input)
	}

	return matches, nil
}

func parseHole(input string) (Hole, string, error) {
	hole, err := parseHolePattern(input, "", "")
	if err == nil {
		return hole, input[len(hole.Pattern):], nil
	}

	hole, rest, err := parseHolePattern(input, "[", "]")
	if err == nil {
		return hole, rest, nil
	}

	hole, rest, err = parseHolePattern(input, "{", "}")
	if err == nil {
		return hole, rest, nil
	}

	hole, rest, err = parseHolePattern(input, "(", ")")
	if err == nil {
		return hole, rest, nil
	}

	hole, rest, err = parseHolePattern(input, "<", ">")
	if err == nil {
		return hole, rest, nil
	}

	hole, rest, err = parseRegexHole(input)
	if err == nil {
		return hole, rest, nil
	}

	return Hole{}, input, fmt.Errorf("failed to parse hole")
}

func parseHolePattern(input, left, right string) (Hole, string, error) {
	leftPattern := regexp.MustCompile(fmt.Sprintf(`^%s([a-zA-Z_']+)%s`, left, right))
	matches := leftPattern.FindStringSubmatch(input)
	if len(matches) == 0 {
		return Hole{}, input, fmt.Errorf("failed to parse hole pattern")
	}

	hole := Hole{
		Pattern:  matches[0],
		Variable: matches[1],
		Kind:     "value",
	}

	return hole, input[len(hole.Pattern):], nil
}

func parseRegexHole(input string) (Hole, string, error) {
	pattern := regexp.MustCompile(`^\$([a-zA-Z_']+)~(.+)\$`)
	matches := pattern.FindStringSubmatch(input)
	if len(matches) == 0 {
		return Hole{}, input, fmt.Errorf("failed to parse regex hole")
	}

	hole := Hole{
		Pattern:  matches[0],
		Variable: matches[1],
		Kind:     "value",
	}

	return hole, input[len(hole.Pattern):], nil
}

func parseConstant(input string) (Constant, string, error) {
	pattern := regexp.MustCompile(`^([^$]+)`)
	matches := pattern.FindStringSubmatch(input)
	if len(matches) == 0 {
		return Constant{}, input, fmt.Errorf("failed to parse constant")
	}

	constant := Constant{
		Value: matches[1],
	}

	return constant, input[len(constant.Value):], nil
}