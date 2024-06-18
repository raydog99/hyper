import re

class Hole:
    def __init__(self, pattern, variable, offset, kind="value"):
        self.pattern = pattern
        self.variable = variable
        self.offset = offset
        self.kind = kind

    def __repr__(self):
        return f"Hole(pattern='{self.pattern}', variable='{self.variable}', offset={self.offset}, kind='{self.kind}')"

class Constant:
    def __init__(self, value):
        self.value = value

    def __repr__(self):
        return f"Constant(value='{self.value}')"

def parse_template(input_str):
    matches = []
    offset = 0

    while input_str:
        for pattern, kind in [
            (r"^(\[\[.*?\]\])", "value"),
            (r"^(\{\{.*?\}\})", "value"),
            (r"^(\(.*?\))", "value"),
            (r"^(\<.*?\>)", "value"),
            (r"^\$([a-zA-Z_']+)~(.+)\$", "value"),
        ]:
            match = re.match(pattern, input_str)
            if match:
                pattern_str, variable = match.groups()[:2]
                hole = Hole(pattern_str, variable, offset, kind)
                matches.append(hole)
                offset += len(pattern_str)
                input_str = input_str[len(pattern_str):]
                break

        else:
            constant_match = re.match(r"^([^$]+)", input_str)
            if constant_match:
                constant_value = constant_match.group(1)
                constant = Constant(constant_value)
                matches.append(constant)
                offset += len(constant_value)
                input_str = input_str[len(constant_value):]
            else:
                break

    return matches