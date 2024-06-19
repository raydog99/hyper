from dataclasses import dataclass
from enum import Enum, auto
from typing import Optional, Union, List

class HoleType(Enum):
    Everything = auto()
    Expression = auto()
    Alphanum = auto()
    NonSpace = auto()
    Line = auto()
    Blank = auto()

Delimited = Union[tuple[Optional[str], Optional[str]], None]
ReservedIdentifiers = List[str]
Delimiter = Union[Delimited, ReservedIdentifiers]

@dataclass
class Hole:
    hole_type: HoleType
    delimiter: Delimiter

@dataclass
class Regex:
    start: str
    sep: str
    end: str

Syntax = List[Union[Hole, Regex]]

@dataclass
class Alias:
    pattern: str
    match_template: str
    rule: Optional[str]

@dataclass
class Metasyntax:
    syntax: Syntax
    identifier: str
    aliases: List[Alias]

def default_syntax() -> Syntax:
    return [
        Regex(":[", '~', "]"),
        Hole(HoleType.Everything, (Some(":["), Some("]"))),
        Hole(HoleType.Expression, (Some(":["), Some(":e]"))),
        Hole(HoleType.Alphanum, (Some(":[["), Some("]]"))),
        Hole(HoleType.NonSpace, (Some(":["), Some(".]"))),
        Hole(HoleType.Line, (Some(":["), Some("\\n]"))),
        Hole(HoleType.Blank, (Some(":[ "), Some("]"))),
        Hole(HoleType.Expression, ["α", "β", "γ", "δ", "ε", "ζ", "η", "θ", "ι", "κ", "λ", "μ", "ξ", "π", "ρ", "ς", "σ", "τ", "υ", "φ", "χ", "ψ", "ω"]),
        Hole(HoleType.Everything, ["Γ", "Δ", "Θ", "Λ", "Ξ", "Π", "Σ", "Φ", "Ψ", "Ω"]),
    ]

def default_identifier() -> str:
    return "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_"

def default_aliases() -> List[Alias]:
    return [Alias("...", ":[_]", None)]

def default_metasyntax() -> Metasyntax:
    return Metasyntax(default_syntax(), default_identifier(), default_aliases())

def create(syntax: Syntax, identifier: str, aliases: List[Alias]) -> Metasyntax:
    return Metasyntax(syntax, identifier, aliases)

def default() -> Metasyntax:
    return create(default_syntax(), default_identifier(), default_aliases())