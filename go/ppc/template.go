package capitan

import (
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"strings"
)

type Variable = string
type Pattern = string
type Offset = int
type Kind uint8

const (
	Value Kind = iota
	Length
	Lines
	OffsetStart
	OffsetEnd
	LineStart
	LineEnd
	ColumnStart
	ColumnEnd
	FilePath
	FileName
	FileDirectory
	Lowercase
	Uppercase
	Capitalize
	Uncapitalize
	UpperCamelCase
	LowerCamelCase
	UpperSnakeCase
	LowerSnakeCase
	External
)

type TemplateElement struct {
	Constant string
	Hole     *Hole
}

type Hole struct {
	Pattern  Pattern
	Variable Variable
	Offset   Offset
	Kind     Kind
}

func attributeToKind(s string) Kind {
	switch s {
	case "value":
		return Value
	case "length":
		return Length
	case "lines":
		return Lines
	case "offset":
		return OffsetStart
	case "offset.start":
		return OffsetStart
	case "offset.end":
		return OffsetEnd
	case "line":
		return LineStart
	case "line.start":
		return LineStart
	case "line.end":
		return LineEnd
	case "column":
		return ColumnStart
	case "column.start":
		return ColumnStart
	case "column.end":
		return ColumnEnd
	case "file":
		return FilePath
	case "file.path":
		return FilePath
	case "file.name":
		return FileName
	case "file.directory":
		return FileDirectory
	case "lowercase":
		return Lowercase
	case "UPPERCASE":
		return Uppercase
	case "Capitalize":
		return Capitalize
	case "uncapitalize":
		return Uncapitalize
	case "UpperCamelCase":
		return UpperCamelCase
	case "lowerCamelCase":
		return LowerCamelCase
	case "UPPER_SNAKE_CASE":
		return UpperSnakeCase
	case "lower_snake_case":
		return LowerSnakeCase
	case "lsif.hover":
		return External
	default:
		panic("invalid attribute " + s)
	}
}

func camelToSnake(s string) string {
	var words []string
	var word []rune
	for _, r := range s {
		if isUpper(r) {
			if len(word) > 0 {
				words = append(words, strings.ToLower(string(word)))
				word = nil
			}
			word = append(word, unicode.ToLower(r))
		} else {
			word = append(word, r)
		}
	}
	if len(word) > 0 {
		words = append(words, strings.ToLower(string(word)))
	}
	return strings.Join(words, "_")
}

func substituteKind(filepath *string, hole *Hole, env map[Variable]string) (string, error) {
	var result string
	var err error

	switch hole.Kind {
	case Value:
		result, err = lookup(env, hole.Variable)
	case Length:
		result, err = length(env, hole.Variable)
	case Lines:
		result, err = lines(env, hole.Variable)
	case OffsetStart:
		result, err = offsetStart(env, hole.Variable)
	case OffsetEnd:
		result, err = offsetEnd(env, hole.Variable)
	case LineStart:
		result, err = lineStart(filepath, env, hole.Variable)
	case LineEnd:
		result, err = lineEnd(filepath, env, hole.Variable)
	case ColumnStart:
		result, err = columnStart(filepath, env, hole.Variable)
	case ColumnEnd:
		result, err = columnEnd(filepath, env, hole.Variable)
	case FilePath:
		if filepath != nil {
			result = *filepath
		}
	case FileName:
		if filepath != nil {
			result = filepath.Base(*filepath)
		}
	case FileDirectory:
		if filepath != nil {
			result = filepath.Dir(*filepath)
		}
	case Lowercase:
		result, err = lowercase(env, hole.Variable)
	case Uppercase:
		result, err = uppercase(env, hole.Variable)
	case Capitalize:
		result, err = capitalize(env, hole.Variable)
	case Uncapitalize:
		result, err = uncapitalize(env, hole.Variable)
	case UpperCamelCase:
		result, err = upperCamelCase(env, hole.Variable)
	case LowerCamelCase:
		result, err = lowerCamelCase(env, hole.Variable)
	case UpperSnakeCase:
		result, err = upperSnakeCase(env, hole.Variable)
	case LowerSnakeCase:
		result, err = lowerSnakeCase(env, hole.Variable)
	case External:
		result, err = external(filepath, env, hole.Variable)
	default:
		err = fmt.Errorf("unsupported kind %v", hole.Kind)
	}

	return result, err
}

func lookup(env map[Variable]string, variable Variable) (string, error) {
	value, ok := env[variable]
	if !ok {
		return "", fmt.Errorf("variable %q not found in environment", variable)
	}
	return value, nil
}

func length(env map[Variable]string, variable Variable) (string, error) {
	value, err := lookup(env, variable)
	if err != nil {
		return "", err
	}
	return fmt.Sprintf("%d", len(value)), nil
}

func lines(env map[Variable]string, variable Variable) (string, error) {
	value, err := lookup(env, variable)
	if err != nil {
		return "", err
	}
	return fmt.Sprintf("%d", strings.Count(value, "\n")+1), nil
}

func offsetStart(env map[Variable]string, variable Variable) (string, error) {
	return "0", nil // Placeholder implementation
}

func offsetEnd(env map[Variable]string, variable Variable) (string, error) {
	value, err := lookup(env, variable)
	if err != nil {
		return "", err
	}
	return fmt.Sprintf("%d", len(value)), nil
}

func lineStart(filepath *string, env map[Variable]string, variable Variable) (string, error) {
	return "1", nil // Placeholder implementation
}

func lineEnd(filepath *string, env map[Variable]string, variable Variable) (string, error) {
	return "1", nil // Placeholder implementation
}

func columnStart(filepath *string, env map[Variable]string, variable Variable) (string, error) {
	return "1", nil // Placeholder implementation
}

func columnEnd(filepath *string, env map[Variable]string, variable Variable) (string, error) {
	return "1", nil // Placeholder implementation
}

func lowercase(env map[Variable]string, variable Variable) (string, error) {
	value, err := lookup(env, variable)
	if err != nil {
		return "", err
	}
	return strings.ToLower(value), nil
}

func uppercase(env map[Variable]string, variable Variable) (string, error) {
	value, err := lookup(env, variable)
	if err != nil {
		return "", err
	}
	return strings.ToUpper(value), nil
}

func capitalize(env map[Variable]string, variable Variable) (string, error) {
	value, err := lookup(env, variable)
	if err != nil {
		return "", err
	}
	return strings.Title(value), nil
}

func uncapitalize(env map[Variable]string, variable Variable) (string, error) {
	value, err := lookup(env, variable)
	if err != nil {
		return "", err
	}
	return strings
}