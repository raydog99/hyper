package efficient

import (
	"fmt"
	"regexp"
	"strings"
)

type RE struct {
	Str   string
	Regex *regexp.Regexp
}

type Lens interface{}

type Del struct {
	RE    RE
	Value string
}

type Store struct {
	RE RE
}

type Value string

type Counter string

type Seq string

type Key struct {
	RE RE
}

type Label string

func concat(l1, l2 Lens, input []string) []string {
	var result []string
	for _, s := range input {
		res1 := applyLens(l1, s)
		for _, r1 := range res1 {
			res2 := applyLens(l2, r1)
			result = append(result, res2...)
		}
	}
	return result
}

func union(l1, l2 Lens, input string) string {
	res1 := applyLens(l1, input)
	if len(res1) > 0 {
		return strings.Join(res1, "")
	}
	return applyLens(l2, input)[0]
}

func repetition(op string, l Lens, input []string) []string {
	var result []string
	for _, s := range input {
		res := applyLens(l, s)
		for _, r := range res {
			result = append(result, r)
		}
		if op == "*" {
			continue
		} else if op == "+" && len(res) == 0 {
			return []string{}
		} else if op == "?" {
			break
		}
	}
	return result
}

func subtree(l Lens, input []string) [][]string {
	var result [][]string
	for _, s := range input {
		res := applyLens(l, s)
		result = append(result, res)
	}
	return result
}

func square(left, body, right Lens, input string) string {
	res1 := applyLens(left, input)
	if len(res1) == 0 {
		return ""
	}
	res2 := applyLens(right, res1[0])
	if len(res2) == 0 {
		return ""
	}
	mid := res2[0][len(res1[0]):]
	midRes := applyLens(body, mid)
	return res1[0] + strings.Join(midRes, "") + res2[0][len(mid):]
}