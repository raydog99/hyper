import re

class RE:
    def __init__(self, value, is_str=True):
        self.value = value
        self.is_str = is_str

    def __repr__(self):
        return f"RE('{self.value}', is_str={self.is_str})"

class Lens:
    pass

class Del(Lens):
    def __init__(self, re, value):
        self.re = re
        self.value = value

    def __repr__(self):
        return f"Del({self.re}, '{self.value}')"

class Store(Lens):
    def __init__(self, re):
        self.re = re

    def __repr__(self):
        return f"Store({self.re})"

class Value(Lens):
    def __init__(self, value):
        self.value = value

    def __repr__(self):
        return f"Value('{self.value}')"

class Counter(Lens):
    def __init__(self, name):
        self.name = name

    def __repr__(self):
        return f"Counter('{self.name}')"

class Seq(Lens):
    def __init__(self, name):
        self.name = name

    def __repr__(self):
        return f"Seq('{self.name}')"

class Key(Lens):
    def __init__(self, re):
        self.re = re

    def __repr__(self):
        return f"Key({self.re})"

class Label(Lens):
    def __init__(self, value):
        self.value = value

    def __repr__(self):
        return f"Label('{self.value}')"

def concat(l1, l2, input_list):
    result = []
    for s in input_list:
        res1 = apply_lens(l1, s)
        for r1 in res1:
            res2 = apply_lens(l2, r1)
            result.extend(res2)
    return result

def union(l1, l2, input_str):
    res1 = apply_lens(l1, input_str)
    if res1:
        return ''.join(res1)
    return apply_lens(l2, input_str)[0]

def repetition(op, l, input_list):
    result = []
    for s in input_list:
        res = apply_lens(l, s)
        result.extend(res)
        if op == '*':
            continue
        elif op == '+' and not res:
            return []
        elif op == '?':
            break
    return result

def subtree(l, input_list):
    result = []
    for s in input_list:
        res = apply_lens(l, s)
        result.append(res)
    return result

def square(left, body, right, input_str):
    res1 = apply_lens(left, input_str)
    if not res1:
        return ""
    res2 = apply_lens(right, res1[0])
    if not res2:
        return ""
    mid = res2[0][len(res1[0]):]
    mid_res = apply_lens(body, mid)
    return res1[0] + ''.join(mid_res) + res2[0][len(mid):]