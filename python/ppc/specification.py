import re
from typing import Optional, List, Union

class Rule:
    pass

class T:
    def __init__(self, match_template: str, rule: Optional[Rule], rewrite_template: Optional[str]):
        self.match_template = match_template
        self.rule = rule
        self.rewrite_template = rewrite_template

def create(rewrite_template: Optional[str], rule: Optional[Rule], match_template: str) -> T:
    return T(match_template, rule, rewrite_template)

class Extracted:
    def to_regex(self) -> str:
        raise NotImplementedError

class Regex(Extracted):
    def __init__(self, content: str):
        self.content = content

    def to_regex(self) -> str:
        return self.content

class ContiguousWhitespace(Extracted):
    def to_regex(self) -> str:
        return r'\s+'

class NonSpace(Extracted):
    def __init__(self, content: str):
        self.content = content

    def to_regex(self) -> str:
        return re.escape(self.content)

def extract(template: str) -> List[Extracted]:
    return []

def to_regex(t: T) -> str:
    extracted = extract(t.match_template)
    return '(' + ''.join(e.to_regex() for e in extracted) + ')'