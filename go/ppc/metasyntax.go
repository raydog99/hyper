package capitan

type HoleType int

const (
    Everything HoleType = iota
    Expression
    Alphanum
    NonSpace
    Line
    Blank
)

type Delimiter struct {
    Start *string
    End   *string
}

type ReservedIdentifiers []string

type Hole struct {
    Type      HoleType
    Delimiter interface{}
}

type Regex struct {
    Start string
    Sep   byte
    End   string
}

type Syntax []interface{}

type Alias struct {
    Pattern       string
    MatchTemplate string
    Rule          *string
}

var defaultSyntax = Syntax{
    Regex{":[", '~', "]"},
    Hole{Everything, Delimiter{start: pstr(":["), end: pstr("]")}},
    Hole{Expression, Delimiter{start: pstr(":["), end: pstr(":e]")}},
    Hole{Alphanum, Delimiter{start: pstr(":[["), end: pstr("]]")}},
    Hole{NonSpace, Delimiter{start: pstr(":["), end: pstr(".]")}},
    Hole{Line, Delimiter{start: pstr(":["), end: pstr("\\n]")}},
    Hole{Blank, Delimiter{start: pstr(":[ "), end: pstr("]")}},
    Hole{Expression, ReservedIdentifiers{"α", "β", "γ", "δ", "ε", "ζ", "η", "θ", "ι", "κ", "λ", "μ", "ξ", "π", "ρ", "ς", "σ", "τ", "υ", "φ", "χ", "ψ", "ω"}},
    Hole{Everything, ReservedIdentifiers{"Γ", "Δ", "Θ", "Λ", "Ξ", "Π", "Σ", "Φ", "Ψ", "Ω"}},
}

var defaultIdentifier = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_"

var defaultAliases = []Alias{
    {Pattern: "...", MatchTemplate: ":[_]", Rule: nil},
}

var defaultMetasyntax = struct {
    Syntax     Syntax
    Identifier string
    Aliases    []Alias
}{
    Syntax:     defaultSyntax,
    Identifier: defaultIdentifier,
    Aliases:    defaultAliases,
}

func create(syntax Syntax, identifier string, aliases []Alias) struct {
    Syntax     Syntax
    Identifier string
    Aliases    []Alias
} {
    return struct {
        Syntax     Syntax
        Identifier string
        Aliases    []Alias
    }{
        Syntax:     syntax,
        Identifier: identifier,
        Aliases:    aliases,
    }
}

func Default() struct {
    Syntax     Syntax
    Identifier string
    Aliases    []Alias
} {
    return create(defaultSyntax, defaultIdentifier, defaultAliases)
}

func pstr(s string) *string {
    return &s
}