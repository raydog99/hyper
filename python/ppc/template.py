import re
from typing import Optional, Tuple

Variable = str
Pattern = str
Offset = int

class AttributeKind:
    Value = 0
    Length = 1
    Lines = 2
    OffsetStart = 3
    OffsetEnd = 4
    LineStart = 5
    LineEnd = 6
    ColumnStart = 7
    ColumnEnd = 8
    FilePath = 9
    FileName = 10
    FileDirectory = 11
    Lowercase = 12
    Uppercase = 13
    Capitalize = 14
    Uncapitalize = 15
    UpperCamelCase = 16
    LowerCamelCase = 17
    UpperSnakeCase = 18
    LowerSnakeCase = 19
    External = 20

class TemplateElement:
    def __init__(self, constant: Optional[str] = None, hole: Optional['Hole'] = None):
        self.constant = constant
        self.hole = hole

class Hole:
    def __init__(self, pattern: Pattern, variable: Variable, offset: Offset, kind: AttributeKind):
        self.pattern = pattern
        self.variable = variable
        self.offset = offset
        self.kind = kind

def attribute_to_kind(s: str) -> AttributeKind:
    if s == "value":
        return AttributeKind.Value
    elif s == "length":
        return AttributeKind.Length
    elif s == "lines":
        return AttributeKind.Lines
    elif s == "offset" or s == "offset.start":
        return AttributeKind.OffsetStart
    elif s == "offset.end":
        return AttributeKind.OffsetEnd
    elif s == "line" or s == "line.start":
        return AttributeKind.LineStart
    elif s == "line.end":
        return AttributeKind.LineEnd
    elif s == "column" or s == "column.start":
        return AttributeKind.ColumnStart
    elif s == "column.end":
        return AttributeKind.ColumnEnd
    elif s == "file" or s == "file.path":
        return AttributeKind.FilePath
    elif s == "file.name":
        return AttributeKind.FileName
    elif s == "file.directory":
        return AttributeKind.FileDirectory
    elif s == "lowercase":
        return AttributeKind.Lowercase
    elif s == "UPPERCASE":
        return AttributeKind.Uppercase
    elif s == "Capitalize":
        return AttributeKind.Capitalize
    elif s == "uncapitalize":
        return AttributeKind.Uncapitalize
    elif s == "UpperCamelCase":
        return AttributeKind.UpperCamelCase
    elif s == "lowerCamelCase":
        return AttributeKind.LowerCamelCase
    elif s == "UPPER_SNAKE_CASE":
        return AttributeKind.UpperSnakeCase
    elif s == "lower_snake_case":
        return AttributeKind.LowerSnakeCase
    elif s == "lsif.hover":
        return AttributeKind.External
    else:
        raise ValueError(f"invalid attribute {s}")

def camel_to_snake(s: str) -> str:
    words = []
    word = []
    for c in s:
        if c.isupper():
            if word:
                words.append(''.join(word).lower())
                word = []
            word.append(c.lower())
        else:
            word.append(c)
    if word:
        words.append(''.join(word).lower())
    return '_'.join(words)

def substitute_kind(filepath: Optional[str], element: TemplateElement, env: dict[Variable, str]) -> Optional[str]:
    if element.constant is not None:
        return None
    else:
        hole = element.hole
        if hole is None:
            return None

        def lookup(var: Variable) -> Optional[str]:
            return env.get(var)

        def match_start(var: Variable) -> Optional[str]:
            return "0" if lookup(var) is not None else None  # Placeholder implementation

        def match_end(var: Variable) -> Optional[str]:
            value = lookup(var)
            return str(len(value)) if value is not None else None  # Placeholder implementation

        def line_start(filepath: Optional[str], var: Variable) -> Optional[str]:
            return "1" if match_start(var) is not None else None  # Placeholder implementation

        def line_end(filepath: Optional[str], var: Variable) -> Optional[str]:
            return "1" if match_end(var) is not None else None  # Placeholder implementation

        def column_start(filepath: Optional[str], var: Variable) -> Optional[str]:
            return "1" if match_start(var) is not None else None  # Placeholder implementation

        def column_end(filepath: Optional[str], var: Variable) -> Optional[str]:
            return "1" if match_end(var) is not None else None  # Placeholder implementation

        def offset_to_line_column(source: str, offset: Offset) -> Tuple[int, int]:
            return (1, 1)  # Placeholder implementation

        def strip_var(var: Variable) -> Optional[Variable]:
            match = re.match(r"^\[(.*?)\]$", var)
            return match.group(1) if match else None

        variable = hole.variable
        kind = hole.kind
        if kind == AttributeKind.Value:
            return lookup(variable)
        elif kind == AttributeKind.Length:
            value = lookup(variable)
            return str(len(value)) if value is not None else None
        elif kind == AttributeKind.Lines:
            value = lookup(variable)
            return str(value.count('\n') + 1) if value is not None else None
        elif kind == AttributeKind.OffsetStart:
            return match_start(variable)
        elif kind == AttributeKind.OffsetEnd:
            return match_end(variable)
        elif kind == AttributeKind.LineStart:
            return line_start(filepath, variable)
        elif kind == AttributeKind.LineEnd:
            return line_end(filepath, variable)
        elif kind == AttributeKind.ColumnStart:
            return column_start(filepath, variable)
        elif kind == AttributeKind.ColumnEnd:
            return column_end(filepath, variable)
        elif kind == AttributeKind.FilePath:
            return filepath
        elif kind == AttributeKind.FileName:
            return filepath.split('/')[-1] if filepath else None
        elif kind == AttributeKind.FileDirectory:
            return '/'.join(filepath.split('/')[:-1]) if filepath else None
        elif kind == AttributeKind.Lowercase:
            value = lookup(variable)
            return value.lower() if value is not None else None
        elif kind == AttributeKind.Uppercase:
            value = lookup(variable)
            return value.upper() if value is not None else None
        elif kind == AttributeKind.Capitalize:
            value = lookup(variable)
            return value.capitalize() if value is not None else None
        elif kind == AttributeKind.Uncapitalize:
            value = lookup(variable)
            return value[0].lower() + value[1:] if value is not None else None
        elif kind == AttributeKind.UpperCamelCase:
            value = lookup(variable)
            return ''.join(word.capitalize() for word in camel_to_snake(value).split('_')) if value is not None else None
        elif kind == AttributeKind.LowerCamelCase:
            value = lookup(variable)
            words = [word.capitalize() for word in camel_to_snake(value).split('_')]
            return (words[0].lower() + ''.join(words[1:])) if words else None
        elif kind == AttributeKind.UpperSnakeCase:
            value = lookup(variable)
            return camel_to_snake(value).upper() if value is not None else None