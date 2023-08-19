# distutils: language=c++

#from __future__ import division

class Expression(object): 
    "A mathematical expression (other than a number)."
    def __init__(self, op, *args): self.op, self.args = op, args
        
    def __add__(self, other):  return Expression('+',  self, other)
    def __sub__(self, other):  return Expression('-',  self, other)
    def __mul__(self, other):  return Expression('*',  self, other)
    def __truediv__(self, other): return Expression('/',  self, other)
    def __pow__(self, other):  return Expression('**', self, other)
    
    def __radd__(self, other): return Expression('+',  other, self)
    def __rsub__(self, other): return Expression('-',  other, self)
    def __rmul__(self, other): return Expression('*',  other, self)
    def __rtruediv__(self, other): return Expression('/',  other, self)
    def __rpow__(self, other): return Expression('**', other, self)
    
    def __neg__(self):         return Expression('-', self)
    def __pos__(self):         return self
    
    def __hash__(self):        return hash(self.op) ^ hash(self.args)
    def __ne__(self, other):   return not (self == other)
    def __eq__(self, other):   return (isinstance(other, Expression) 
                                       and self.op == other.op 
                                       and self.args == other.args)
    def __repr__(self):
        "A string representation of the Expression."
        op, args = self.op, self.args
        n = arity(self)
        if n == 0: 
            return op
        if n == 2: 
            return '({} {} {})'.format(args[0], op, args[1])
        if n == 1:
            arg = str(args[0])
            if arg.startswith('(') or op in {'+', '-'}:
                return '{}{}'.format(op, arg)
            else:
                return '{}({})'.format(op, arg)

def arity(expr):
    "The number of sub-expressions in this expression."
    if isinstance(expr, Expression):
        return len(expr.args)
    else:
        return 0
    
a,    b,   c,   d,   u,   v,   w,   x,   y,   z,   pi ,  e = map(Expression, [
'a', 'b', 'c', 'd', 'u', 'v', 'w', 'x', 'y', 'z', 'pi', 'e'])

def D(y, x=x):
    """Return the symbolic derivative, dy/dx.
    Handles binary operators +, -, *, /, and unary -, as well as
    transcendental trig and log functions over variables and constants."""
    if y == x:            
        return 1        # d/dx (x) = 1
    if arity(y) == 0:
        return 0        # d/dx (c) = 0
    if arity(y) == 1:
        op, (u,) = y.op, y.args
        if op == '+':  return D(u, x)
        if op == '-':  return -D(u, x)
        if u  != x:    return D(y, u) * D(u, x) ## CHAIN RULE
        if op == sin:  return cos(x)
        if op == cos:  return -sin(x)
        if op == tan:  return sec(x) ** 2
        if op == cot:  return -csc(x) ** 2
        if op == sec:  return sec(x) * tan(x)
        if op == csc:  return -csc(x) * cot(x)
        if op == ln:   return 1 / x  
    if arity(y) == 2: 
        op, (u, v) = y.op, y.args
        if op == '+':  return D(u, x) + D(v, x)
        if op == '-':  return D(u, x) - D(v, x)
        if op == '*':  return D(u, x) * v +  D(v, x) * u
        if op == '/':  return (v * D(u, x) - u * D(v, x)) / v ** 2
        if op == '**': return (v * u**(v-1) if u == x and isinstance(v, int) else
                               v * u**(v-1) * D(u, x) + u**v * ln(u) * D(v, x))
    raise ValueError("D can't handle this: " + str(y))



class Function(Expression):
    "A mathematical function of one argument, like sin or ln."
    def __call__(self, x): return Expression(self, x)

sin,    cos,   tan,   cot,   sec,   csc,   ln,   sqrt = map(Function, [
'sin', 'cos', 'tan', 'cot', 'sec', 'csc', 'ln', 'sqrt'])






def simp(y):
    "Simplify an expression."
    if arity(y) == 0:
        return y 
    y = Expression(y.op, *map(simp, y.args))  ## Simplify the sub-expressions first
    op = y.op
    if arity(y) == 1:
        (u,) = y.args
        if y in simp_table:
            return simp_table[y]
        if op == '+':
            return u                         # + u = u
        if op == '-':
            if arity(u) == 1 and u.op == '-':
                return u.args[0]             # - - w = w
    if arity(y) == 2:
        (u, v) = y.args
        if evaluable(u, op, v): # for example, (3 + 4) simplifies to 7
            return eval(str(u) + op + str(v), {})
        if op == '+':
            if u == 0: return v              # 0 + v = v
            if v == 0: return u              # u + 0 = u
            if u == v: return 2 * u          # u + u = 2 * u
        if op == '-':
            if u == v: return 0              # u - v = 0
            if v == 0: return u              # u - 0 = u
            if u == 0: return -v             # 0 - v = -v
        if op == '*':
            if u == 0 or v == 0: return 0    # 0 * v = u * 0 = 0
            if u == 1: return v              # 1 * v = v
            if v == 1: return u              # u * 1 = u
            if u == v: return u ** 2         # u * u = u^2
        if op == '/':
            if u == 0 and v == 0: return undefined # 0 / 0 = undefined
            if u == v: return 1              # u / u = 1
            if v == 1: return u              # u / 1 = u
            if u == 0: return 0              # 0 / v = 0
            if v == 0: return infinity       # u / 0 = infinity
        if op == '**':
            if v == 1: return u              # u ** 1 = u
            if u == v == 0: return undefined # 0 ** 0 = undefined
            if v == 0: return 1              # u ** 0 = 1  

    # If no rules apply, return y unchanged.
    return y

from numbers import Number

# Deal with infinity and with undefined numbers (like infinity minus infinity)
infinity = float('inf') 
undefined = nan = (infinity - infinity)

# Table of known exact values for certain functions.
# Use this, for example, to simplify sin(pi) to 0 or ln(e) to 1.

simp_table = {
    sin(0): 0, sin(pi): 0,  sin(pi/2): 1,        sin(2*pi): 0,
    cos(0): 1, cos(pi): -1, cos(pi/2): 1,        cos(2*pi): 1,
    tan(0): 0, tan(pi): 0,  tan(pi/2): infinity, tan(2*pi): 0, tan(pi/4): 1,
    ln(1): 0,  ln(e): 1,    ln(0): -infinity}

def evaluable(u, op, v):
    "Can we evaluate (u op v) to a number? True if u and v are numbers, and not a special case like 0^0."
    return (isinstance(u, Number) and isinstance(v, Number)
            and not (op == '/' and (v == 0 or u % v != 0))
            and not (op == '^' and (u == v == 0)))


#final function, using to diff
def diff(y, x=x):
    return simp(D(y, x))








    