#!/usr/bin/env python3
import ast
import re as py_re
import math
import cmath
import fractions
import decimal
import string
import  mpmath

try:
    import sympy
    import sympy.abc as sympy_abc
    sympy_abc = dir(sympy_abc)
    sympy_abc = [i for i in sympy_abc if i not in ["__annotations__", "__builtins__", '__cached__', '__doc__', '__file__', '__loader__', '__name__', '__package__', '__spec__']]
except Exception:
    import math as sympy
    sympy_abc = []


def is_number(num):
    match num:
        case bool():
            pass
        case int() | float() | complex() | fractions.Fraction() | decimal.Decimal() | fractions.Decimal():
            return True
    return False

def join_and(words, **kwargs):
    jword = "and"
    if isinstance(kwargs.get("jword"), str):
            jword = kwargs["jword"]
    if len(words) > 2:
        return f"{', '.join(words[:-1])}, {jword} {words[-1]}"
    else:
        
        return f' {jword} '.join(words)

def is_ast(node):
    match node:
        case bool():
            pass
        case ast.AST():
            return True
        case str(x) if is_operator(x):
            return False
        case str():
            return is_ast(to_ast(node))
        case _ if is_number(node):
            return True
    return False

def to_ast(node, **kwargs):
    """
    Converts various types of input nodes into an abstract syntax tree (AST).

    Args:
        node: The input node (can be int, float, fraction, decimal, or str).
        mode (optional): The parsing mode ("eval" by default).

    Returns:
        The AST corresponding to the input node.
    """
    if is_number(node):
        # Convert numeric values to string representation
        return to_ast(str(node))
    elif isinstance(node, str):
        mode = kwargs.get("mode", "eval")
        # Parse the string as an expression
        tree = ast.parse(node, mode=mode)
        if isinstance(tree, ast.Expression):
            # Return the body of the expression
            return tree.body
    elif isinstance(node, ast.AST):
        return node
    else:
        raise ValueError("Unsupported input type")

def from_ast(node, **kwargs):
    match node:
        case ast.Constant():
            return node.value
        case str() | sympy.Basic() | sympy.Expr():
            return node
        case ast.AST():
            return ast.unparse(node)
        case _ if is_number(node):
            return node
        case _:
            match type(node).__name__:
                case "RandUnit":
                    return from_ast(node.number)


class Expr_Parse(ast.NodeVisitor):
    def __init__(self, expr_str:str = None):
        super(Expr_Parse, self).__init__()
        self.expr_str = expr_str
        self.expr_ast = None
        if is_ast(expr_str):
            self.expr_ast = to_ast(expr_str)
        
        if isinstance(self.expr_ast, ast.AST):
            self.expr_ast = self.visit(self.expr_ast)
    
    
    def isFrac(self, numerator, denominator):
        match numerator:
            case ast.Constant():
                numerator = numerator.value
            case ast.UnaryOp():
                while True:
                    if isinstance(numerator.operand, ast.Constant):
                        numerator = numerator.operand
                        break
                    elif isinstance(numerator.operand, ast.UnaryOp):
                        numerator = numerator.operand
                    else:
                        break
        match denominator:
            case ast.Constant():
                denominator = denominator.value
            case ast.UnaryOp():
                while True:
                    if isinstance(denominator.operand, ast.Constant):
                        denominator = denominator.operand
                        break
                    elif isinstance(denominator.operand, ast.UnaryOp):
                        denominator = denominator.operand
                    else:
                        break
        
        if isinstance(numerator, ast.Constant) and isinstance(denominator, ast.Constant):
            if isinstance(numerator.value, int) and isinstance(numerator.value, int):
                return True

        return False

    def visit_BinOp(self, node):
        if isinstance(node.left, ast.BinOp):
            node.left = self.visit(node.left)
        if isinstance(node.right, ast.BinOp):
            node.right = self.visit(node.right)
        match node.op:
            case ast.Div() if self.isFrac(node.left, node.right):
                return ast.Call(func=ast.Name(id="Fraction", ctx=ast.Load()), args=[node.left, node.right], keywords=[])

        return node
    
    def __str__(self):
        if is_ast(self.expr_ast):
            return ast.unparse(self.expr_ast)
        elif self.expr_ast:
            return str(self.expr_ast)
        return str(self.expr_str)
    
    def __repr__(self):
        if is_ast(self.expr_ast):
            return ast.dump(self.expr_ast, indent=4)
        elif self.expr_ast:
            return repr(self.expr_ast)
        return repr(self.expr_str)

class Str2Eval(ast.NodeVisitor):
    def __init__(self, expr_str:str = None, module_name="sympy", **kwargs):
        super(Str2Eval, self).__init__()
        self.expr_str = expr_str
        self.module_name = module_name
        if kwargs:
            self.kwargs = kwargs
            self.__dict__.update(self.kwargs)
        if is_ast(expr_str):
            self.expr_ast = to_ast(expr_str)

        match self.module_name:
            case "sympy":
                self.module_methods = dir(sympy)
            case "cmath":
                self.module_methods = dir(cmath)
            case _:
                self.module_name = "math"
                self.module_methods = dir(math)

        if isinstance(self.expr_ast, ast.AST):
            self.expr_ast = self.visit(self.expr_ast)
    
    def __getattr__(self, key):
        match key:
            case str():
                return self.__dict__.get(key)
            case int() if len(self.args):
                return self.__dict__["args"][key]
            case _:
                raise AttributeError(f"can't get {key}")

    def __setattr__(self, key, item):
        self.__dict__[f"{key}"] = item

    def __delattr__(self, key):
        match key:
            case str():
                self.__dict__.__delitem__(key)
            case int(x) if len(self.args) > x:
                del self.__dict__["args"][key]
            case _:
                raise AttributeError(f"can't get {key}")

    def ModuleName(self, name):
        if name in self.module_methods:
            return self.module_name
        elif name in dir(extra_math):
            return "extra_math"
        elif name in dir(sympy):
            return "sympy"
        elif name in dir(math):
            return "math"
        elif name in dir(cmath):
            return "cmath"
        

    def visit_Call(self, node):
        match node.func.id if isinstance(node.func, ast.Name) else None:
            case "Fraction":
                return ast.Call(
                    func=ast.Attribute(
                        value=ast.Name(id="fractions", ctx=ast.Load()),
                        attr="Fraction",
                        ctx=ast.Load()),
                    args=node.args,
                    keywords=node.keywords)
            case "Decimal":
                return ast.Call(
                    func=ast.Attribute(
                        value=ast.Name(id="Decimal", ctx=ast.Load()),
                        attr="Decimal",
                        ctx=ast.Load()),
                    args=node.args,
                    keywords=node.keywords)
            case str(x) if self.ModuleName(x):
                module_name = self.ModuleName(x)
                node.args = [self.visit(i) for i in node.args]
                node.keywords = [self.visit(i) for i in node.keywords]
                return ast.Call(
                    func=ast.Attribute(
                        value=ast.Name(id=module_name, ctx=ast.Load()),
                        attr=x,
                        ctx=ast.Load()),
                    args=node.args,
                    keywords=node.keywords)
        return node
    def visit_Compare(self, node):
        node.left = self.visit(node.left)
        if self.equation and len(node.comparators) == 1 and len(node.ops) == 1:
            if isinstance(node.ops[0], ast.Eq):
                right = self.visit(node.comparators[0])
                return ast.Call(func=ast.Attribute(value=ast.Name(id='sympy', ctx=ast.Load()), attr='Eq',ctx=ast.Load()),
                args=[node.left,right],keywords=[])
        
        node.comparators = [self.visit(i) for i in node.comparators]
        return node

    def visit_UnaryOp(self, node):
        node.operand = self.visit(node.operand)
        return node

    def visit_BinOp(self, node):
        node.left = self.visit(node.left)
        node.right = self.visit(node.right)
        match node.op:
            case ast.BitXor():
                node.op = ast.Pow()
            case ast.BitAnd():
                node.op = ast.Add()
            case ast.BitOr():
                node.op = ast.Sub()
            case ast.FloorDiv() if self.floordiv == False:
                node.op = ast.Div()
        return node
    
    def visit_Constant(self, node):
        return node
    
    def visit_arg(self, node):
        print(node)
        return node

    def visit_Name(self, node):
        if self.ModuleName(node.id):
            return ast.Attribute(
                value=ast.Name(id=self.ModuleName(node.id), ctx=ast.Load()),
                attr=node.id,
                ctx=ast.Load())
        elif node.id in sympy_abc and self.symbols != False:
            return ast.Call(
                func=ast.Attribute(
                    value=ast.Name(id='sympy', ctx=ast.Load()),
                    attr='Symbol',
                    ctx=ast.Load()),
                args=[
                    ast.Constant(value=node.id)],
                keywords=[])
    
        return node
    
    def visit_keyword(self, node):
        node.value = self.visit(node.value)
        return node
    
    def __str__(self):
        if is_ast(self.expr_ast):
            return ast.unparse(self.expr_ast)
        elif self.expr_ast:
            return str(self.expr_ast)
        return str(self.expr_str)
    
    def __repr__(self):
        if is_ast(self.expr_ast):
            return ast.dump(self.expr_ast, indent=4)
        elif self.expr_ast:
            return repr(self.expr_ast)
        return repr(self.expr_str)

def Eval(expression, **kwargs):
    match expression:
        case str() if is_operator(expression):
            return Mathematical_operator(expression, mode="python")
        case str():
            expression = Str2Eval(expression, equation=kwargs.get("equation", True), floordiv=kwargs.get("floordiv", True))
            expression = eval(str(expression))
            if kwargs.get("mode") != None:
                return Eval(expression, **kwargs)
            match expression:
                case float() | sympy.Float():
                    expression = float(expression)
                    if expression.is_integer():
                        return int(expression)
                    return expression
            return expression
        case ast.AST():
            return Eval(ast.unparse(expression), **kwargs)
        case sympy.Expr() | sympy.Basic():
            expression = sympy.simplify(expression)
            match kwargs.get("mode"):
                case "solve":
                    eq2 = [expression]
                    if isinstance(kwargs.get("eq"), (list, set, tuple)):
                        eq2 = [Eval(i) for i in kwargs["eq"]]
                        eq2.insert(0, expression)
                    elif isinstance(kwargs.get("eq"), (str, ast.AST, sympy.Expr, sympy.Basic, Str2Eval)):
                        eq2 = [expression, Eval(kwargs["eq"])]
                    eq2 = tuple(eq2)
                    
                    if isinstance(kwargs.get("symbols"), tuple):
                        symbols = [Eval(i) for i in kwargs["symbols"]]
                        return sympy.solve(eq2, tuple(symbols))
                    return sympy.solve(eq2)
                case str(x) if x in dir(sympy):
                    return getattr(sympy, kwargs["mode"])(expression)
                case "str":
                    return str(expression)
                case _:
                    return expression
        case list() as expr2str:
            i = 0
            expr_str = ""
            while i < len(expr2str):
                match expr2str[i]:
                    case list():
                        expr_str += f'({Eval(expr2str[i])})'
                    case {"name" : "frac", "p" : p, "q" : q}:
                        if isinstance(p, list):
                            p = f'({Eval(p)})'
                        if isinstance(q, list):
                            q = f'({Eval(q)})'
                        expr_str  += f"(p/q)"
                    case _ if is_operator(str(expr2str[i])):
                        if str(expr2str[i]) == "//" and kwargs.get("mode") != "str":
                            expr_str += " / "
                        else:
                            expr_str += f' {expr2str[i]} '
                    case _ if kwargs.get("mode") == "eval":
                        expr_str += f'({expr2str[i]})'
                    case _:
                        expr_str += f'{expr2str[i]}'
                i += 1
            if kwargs.get("mode") == "eval":
                print(expr_str)
                return Eval(expr_str)
            return expr_str
        case _ if is_number(expression):
            match kwargs.get("mode"):
                case "str":
                    return str(expression)
            return expression
        case _:
            return Eval(str(expression), **kwargs)
        

class LatexVisitor(ast.NodeVisitor):
    """
    Parameters
    ----------

    simplify_multipliers: bool
        if ``True``, simplify expression if multiplier is a float. Ex::

            2*a -> 2a
            a*3.5 -> 3.5a

        see :meth:`~pytexit.core.core.LatexVisitor.visit_BinOp` for more
        information. Default ``True``.

    simplify_fractions: bool
        if ``True``, simplify fractions.
        see :meth:`~pytexit.core.core.LatexVisitor.visit_BinOp` for more
        information. Default ``False``.

    simplify_ints: bool
        see :meth:`~pytexit.core.core.LatexVisitor.visit_BinOp` for more
        information. Default ``False``.

    """

    def __init__(
        self,
        dummy_var="u",
        upperscript="ˆ",
        lowerscript="_",
        verbose=False,
        simplify_multipliers=True,
        simplify_fractions=False,
        simplify_ints=True,
        tex_multiplier=r" \times ",
        **kwargs
    ):

        super(LatexVisitor, self).__init__()
        # super().__init__()  # doesn't work in Python 2.x
        self.dummy_var = dummy_var

        self.upper = upperscript
        self.lower = lowerscript

        self.verbose = verbose
        self.simplify_multipliers = simplify_multipliers
        self.simplify_fractions = simplify_fractions
        self.simplify_ints = simplify_ints
        self.tex_multiplier = tex_multiplier
        
        if kwargs:
            self.kwargs = kwargs
            self.__dict__.update(self.kwargs)

        self.precdic = {
            "Pow": 700,
            "Div": 400,
            "FloorDiv": 400,
            "Mult": 400,
            "Invert": 800,
            "Compare": 300,
            "UAdd": 800,
            "Not": 800,
            "USub": 800,
            "Num": 1000,
            "Constant": 1000,
            "Assign": 300,
            "Sub": 300,
            "Add": 300,
            "Mod": 500,
            "ListComp": 1000,
            "list": 1000,
            "Call": 1000,
            "Name": 1000,
        }
    def __getattr__(self, key):
        match key:
            case str():
                return self.__dict__.get(key)
            case int() if len(self.args):
                return self.__dict__["args"][key]
            case _:
                raise AttributeError(f"can't get {key}")

    def __setattr__(self, key, item):
        self.__dict__[f"{key}"] = item

    def __delattr__(self, key):
        match key:
            case str():
                self.__dict__.__delitem__(key)
            case int(x) if len(self.args) > x:
                del self.__dict__["args"][key]
            case _:
                raise AttributeError(f"can't get {key}")

    def looks_like_int(self, a):
        """Check if the input ``a`` looks like an integer"""

        if self.simplify_ints:
            try:
                return float(str(a).split(".")[1]) == 0.0
            except (IndexError, ValueError):
                return False
        else:
            return False

    def prec(self, n):
        if n.__class__.__name__ in self.precdic:
            return self.precdic[n.__class__.__name__]
        else:
            return getattr(
                self, "prec_" + n.__class__.__name__, getattr(self, "generic_prec")
            )(n)

    def visit_ListComp(self, n, kwout=False):
        """Analyse a list comprehension
        Output :
        - kw : used by some functions to display nice formulas (e.g : sum)
        - args : standard output to display a range in other cases
        """

        kw = {}

        #        lc = n.args[0]
        comp = n.generators[0]
        kw["iterator"] = self.visit(comp.target)
        f = self.visit(comp.iter.func)

        if f == "range":
            if len(comp.iter.args) > 1:
                kw["min"] = self.visit(comp.iter.args[0])
                kw["max"] = self.visit(comp.iter.args[1])
            else:
                kw["min"] = 0
                kw["max"] = self.visit(comp.iter.args[0])
            # Remove 1 for range max
            try:
                kw["max"] = int(kw["max"]) - 1
            except ValueError:
                if kw["max"].endswith(r"+1"):
                    # write 'sum([... range(N+1)])' as (sum^N)
                    kw["max"] = kw["max"][:-2]
                else:
                    # write 'sum([... range(N)])' as (sum^N-1)
                    kw["max"] = r"{0}-1".format(kw["max"])
            kw["content"] = self.visit(n.elt)

        args = r"%s, %s=%s..%s" % (kw["content"], kw["iterator"], kw["min"], kw["max"])

        if kwout:
            return args, kw
        else:
            return args

    def visit_list(self, n):
        self.generic_visit(n)

    def visit_Attribute(self, n):
        return n.attr

    def visit_Set(self, n):
        elts = ", ".join(map(self.visit, n.elts))
        return r"\left\{{{0}\right\}}".format(elts)

    def visit_Call(self, n):
        
        """Node details : n.args, n.func, n.keywords, n.kwargs"""
        
        func = self.visit(n.func)

        # Deal with list comprehension and complex formats
        if len(n.args) > 0:
            blist = isinstance(n.args[0], ast.ListComp)
        else:
            blist = False

        if blist:
            args, kwargs = self.visit_ListComp(n.args[0], kwout=True)
        else:
            args = ", ".join(map(self.visit, n.args))
        
        parameter_args = True if len(n.args) else False
        parameter_kwargs = True if len(n.keywords) else False
        # Usual math functions
        match func:
            case "tridegree":
                if parameter_args:
                    if not isinstance(n.args[0], ast.Constant):
                        args = self.parenthesis(args)
                return r"{0}^{{\\circ}}".format(args)
            case "percent":
                if parameter_args:
                    if not isinstance(n.args[0], ast.Constant):
                        args = self.parenthesis(args)
                return r"{0}\%".format(args)
            case "Eq" if len(n.args) in (1, 2):
                args = "=".join(map(self.visit, n.args))
                if len(n.args) == 2:
                    return args
                else:
                    return f"{args} = 0"
            case "Symbol":
                return args
            case "gcds":
                return join_and(list(map(self.visit, n.args)), jword=self.join_word)
            case "cos" | "sin" | "tan" | "cosh" | "sinh" | "tanh" | "cosec" | "csc" | "sec" | "cot":
                return r"\mathrm{{{0}}}{1}".format(func, self.parenthesis(args))
            case "sqrt" | "isqrt":
                return self.sqrt(args)
            
            # by default log refers to log10 in Python. Unless people import it as
            # ln
            case "log" | "ln":
                return r"\ln%s" % self.parenthesis(args)
            case "log10":
                return r"\log%s" % self.parenthesis(args)
            case "arccos" | "acos":
                return r"\arccos%s" % self.parenthesis(args)
            case "arcsin" | "asin":
                return r"\arcsin%s" % self.parenthesis(args)
            case "arctan" | "atan":
                return r"\arctan%s" % self.parenthesis(args)
            case "arcsinh":
                return r"\sinh^{-1}%s" % self.parenthesis(args)
            case "arccosh":
                return r"\cosh^{-1}%s" % self.parenthesis(args)
            case "arctanh":
                return r"\tanh^{-1}%s" % self.parenthesis(args)
            case "power" | "pow" | "Pow":
                args = [arg.strip() for arg in args.split(",")]
                if "+" in args[0] or "-" in args[0]:
                    return self.power(self.parenthesis(args[0]), args[1])
                else:
                    return self.power(args[0], args[1])
                    
            case "divide" | "Fraction" | "Rational":
                args = [arg.strip() for arg in args.split(",")]
                return self.division(args[0], args[1])
            case "FractionSlash" if len(n.args) == 2:
                return "/".join(map(self.visit, n.args))
            case "Abs" | "abs" | "fabs":
                return r"\left|%s\right|" % args
            case "exp":
                return r"e^{%s}" % args
            # Additionnal functions (convention names, not in numpy library)
            case "kronecher" | "kron":
                return r"\delta_{%s}" % args
            # Integrals
            # TODO : add this integral in a visit_tripOp function???
            case "quad":
                (f, a, b) = list(map(self.visit, n.args))
                return r"\int_{%s}^{%s} %s%s d%s" % (
                    a,
                    b,
                    f,
                    self.parenthesis(self.dummy_var),
                    self.dummy_var,
                )
            case "sum":
                if blist:
                    return "\\sum_{%s=%s}^{%s} %s" % (
                    kwargs["iterator"],
                    kwargs["min"],
                    kwargs["max"],
                    kwargs["content"],
                )
                else:
                    return r"\sum %s" % (args)
            case "f" | "g" | "h":
                return r"%s{%s}" % (func, self.parenthesis(args))
            case _:
                return self.operator(func, args)
            
            
    def visit_Name(self, n):
        """Special features:
        - Recognize underscripts in identifiers names (default: underscore)
        - Recognize upperscripts in identifiers names (default: ˆ, valid in Python3)
        Note that using ˆ is not recommended in variable names because it may
        be confused with the operator ^, but in some special cases of extensively
        long formulas with lots of indices, it may help the readability of the
        code
        """

        u = n.id.count(self.upper)
        if u > 1:
            if self.verbose:
                warn("Only one upperscript character supported per identifier")

        def build_tree(expr, level=1):
            """Builds a tree out of a valid Python identifier, according to the
            following proposed formalism:

            Formalism
            ----------
                Python -> Real

                k_i_j  -> k_i,j
                k_i__j -> k_(i_j)
                k_iˆj -> k_i^j
                k_iˆˆj -> k_(i^j)
                k_i__1_i__2ˆj__1ˆˆj__2 -> k_(i_1,i_2)^(j_1,j_2)

            Even if one may agree that this last expression isn't a very
            readable variable name.

            The idea behind this is that I want my Python formula to be the
            same objects as the LaTeX formula I write in my reports / papers

            It allows me to:
            - gain time
            - check my Python formula (once printed LaTeX is much more readable
            that a multiline Python expression)

            """

            #            sep0 = '[{0}][{1}]'.format(self.lower,self.upper)
            sep = "[{0}{1}]".format(self.lower, self.upper)
            s = py_re.split(
                r"(?<!{0})({0}{{{1}}})(?!{0})".format(sep, level), expr
            )  # Also returns the pattern n
            t = {}  # build tree
            if self.verbose:
                uprint("  " * (level - 1), "val:", self.convert_symbols(s[0]))
            t["val"] = self.convert_symbols(s[0])
            t["low"] = []
            t["up"] = []
            for i in range(1, len(s), 2):
                p = s[i]
                if p == self.lower * level:
                    if self.verbose:
                        uprint("  " * (level - 1), "low:", s[i + 1])
                    t["low"].append(build_tree(s[i + 1], level + 1))
                elif p == self.upper * level:
                    if self.verbose:
                        uprint("  " * (level - 1), "up:", s[i + 1])
                    t["up"].append(build_tree(s[i + 1], level + 1))
                else:
                    raise ValueError("Undetected separator")
            return t

        def read_tree(t):
            """Write a LaTeX readable name"""
            r = t["val"]
            if t["low"] != []:
                #                child = [self.group(read_tree(tc)) for tc in t['low']]
                child = [read_tree(tc) for tc in t["low"]]
                r += "_{0}".format(self.group(",".join(child)))
            if t["up"] != []:
                #                child = [self.group(read_tree(tc)) for tc in t['up']]
                child = [read_tree(tc) for tc in t["up"]]
                r += "^{0}".format(self.group(",".join(child)))
            return r

        return read_tree(build_tree(n.id))

    #    def convert_underscores(self, expr):
    #
    #        s = expr.split(self.lower)
    #
    #        for i, m in enumerate(s):
    #            s[i] = self.convert_symbols(m)
    #
    #        return s

    def convert_symbols(self, expr):
        m = expr
        # Standard greek letters
        if m in [
            "alpha",
            "beta",
            "gamma",
            "delta",
            "epsilon",
            "zeta",
            "eta",
            "theta",
            "iota",
            "kappa",
            "mu",
            "nu",
            "xi",
            "pi",
            "rho",
            "sigma",
            "tau",
            "phi",
            "chi",
            "psi",
            "omega",
            "Gamma",
            "Delta",
            "Theta",
            "Lambda",
            "Xi",
            "Pi",
            "Sigma",
            "Upsilon",
            "Phi",
            "Psi",
            "Omega",
        ]:
            m = r"\%s" % m

        elif m in ["eps"]:
            m = r"\epsilon"

        elif m in [
            "lbd"
        ]:  # lambda is not a valid identifier in Python so people use other things
            m = r"\lambda"

        elif m in ["Lbd"]:
            m = r"\Lambda"

        elif m in ["inf", "infinity", "infty"]:
            m = r"\infty"

        # Replace Delta even if not full word  - Allow for expressions such as
        # ΔE
        elif "Delta" in m:
            m = m.replace("Delta", "\\Delta ")

        return m

    def visit_UnaryOp(self, n):
        # Note: Unary operator followed by a power needs no parenthesis
        if self.prec(n.op) > self.prec(n.operand) and not (
            hasattr(n.operand, "op") and isinstance(n.operand.op, ast.Pow)
        ):
            return r"{0}{1}".format(
                self.visit(n.op), self.parenthesis(self.visit(n.operand))
            )
        else:
            return r"{0}{1}".format(self.visit(n.op), self.visit(n.operand))

    def prec_UnaryOp(self, n):
        return self.prec(n.op)

    def visit_BinOp(self, n):

        if self.prec(n.op) > self.prec(n.left):
            
            left = self.parenthesis(self.visit(n.left))
        elif isinstance(n.op, ast.Pow) and self.prec(n.op) == self.prec(n.left):
            # Special case for power, which needs parentheses when combined to the left
            left = self.parenthesis(self.visit(n.left))
        else:
            left = self.visit(n.left)

        if self.prec(n.op) > self.prec(n.right):
            right = self.parenthesis(self.visit(n.right))
        else:
            right = self.visit(n.right)

        # Special binary operators
        match n.op:
            case ast.Div():
                if self.simplify_fractions:
                    left_is_int = self.looks_like_int(left)
                    right_is_int = self.looks_like_int(right)
                    if left_is_int or right_is_int:
                        if left_is_int and right_is_int:
                            return self.division(
                                "%d" % int(float(left)), "%d" % int(float(right))
                            )
                        elif left_is_int:
                            return self.division(
                                "%d" % int(float(left)), self.visit(n.right)
                            )
                        else:
                            return self.division(
                                self.visit(n.left), "%d" % int(float(right))
                            )
                return self.division(self.visit(n.left), self.visit(n.right))
            case ast.FloorDiv():
                return f"{left} \\div {right}"
            case ast.Pow():
                return self.power(left, self.visit(n.right))
            case ast.Mult():
                def looks_like_float(a):
                    # Check if 'a' looks like a float
                    # Detect: 'float', '{float}', 'int', '{int}'
                    if a.startswith("{"):
                        a = a[1:].split("}")[0]
                    try:
                        float(a)
                        return True
                    except ValueError:
                        return False

                left_is_float = looks_like_float(left)
                right_is_float = looks_like_float(right)

                # Get multiplication operator. Force x if floats are involved
                if left_is_float or right_is_float:
                    operator = self.tex_multiplier
                else:  # get standard Mult operator (see visit_Mult)
                    operator = self.visit(n.op)

                if self.simplify_multipliers:

                    # We simplify in some cases, for instance: a*2 -> 2a
                    # First we need to know if both terms start with numbers
                    
                    if left[0] == "-" or left[0] == "{":
                        left_starts_with_digit = left[1].isdigit()
                    else:
                        left_starts_with_digit = left[0].isdigit()
                    if right[0] == "-" or right[0] == "{":
                        right_starts_with_digit = right[1].isdigit()
                    else:
                        right_starts_with_digit = right[0].isdigit()

                    # Simplify
                    # ... simplify (a*2 --> 2a)
                    if right_is_float and not left_starts_with_digit:
                        return r"{0}{1}".format(right, left)
                    # ... simplify (2*a --> 2a)
                    elif left_is_float and not right_starts_with_digit:
                        return r"{0}{1}".format(left, right)
                    else:
                        return r"{0}{1}{2}".format(left, operator, right)
                elif self.simplify_multipliers == None:
                    return r"{0} \times {1}".format(left, right)
                else:
                    return r"{0}{1}{2}".format(left, operator, right)
            case _:
                return r"{0}{1}{2}".format(left, self.visit(n.op), right)

    def prec_BinOp(self, n):
        return self.prec(n.op)

    def visit_Sub(self, n):
        return " - "

    def visit_Add(self, n):
        return " + "

    def visit_Mult(self, n):
        return r" "

    def visit_Mod(self, n):
        return "\\bmod"

    def visit_LShift(self, n):
        return self.operator("shiftLeft")

    def visit_RShift(self, n):
        return self.operator("shiftRight")

    def visit_BitOr(self, n):
        return self.operator("or")

    def visit_BitXor(self, n):
        return self.operator("xor")

    def visit_BitAnd(self, n):
        return self.operator("and")

    def visit_Invert(self, n):
        return self.operator("invert")

    def visit_Not(self, n):
        return "\\neg "

    def visit_UAdd(self, n):
        return "+"

    def visit_USub(self, n):
        return "-"

    def visit_Constant(self, n):
        return str(n.value)

    # New visits
    def visit_Assign(self, n):
        "Rewrite Assign function (instead of executing it)"
        return r"%s=%s" % (self.visit(n.targets[0]), self.visit(n.value))

    
    def visit_Compare(self, n):
        "Rewrite Compare function (instead of executing it)"

        def visit_Op(op):
            "Note : not called by visit like other visit functions"
            match op:
                case ast.Lt():
                    return "<"
                case ast.LtE():
                    return "<="
                case ast.Gt():
                    return ">"
                case ast.GtE():
                    return ">="
                case ast.Eq():
                    return "="
                case _:
                    raise ValueError("Unknown comparator", op.__class__)

        return r"%s%s" % (
            self.visit(n.left),
            "".join(
                [
                    "%s%s" % (visit_Op(n.ops[i]), self.visit(n.comparators[i]))
                    for i in range(len(n.comparators))
                ]
            ),
        )

    # Default
    def generic_visit(self, n):
        match n:
            case ast.AST():
                return r"" % (
                n.__class__.__name__,
                ", ".join(map(self.visit, [getattr(n, f) for f in n._fields])),
                )
            case _:
                return str(n)

    def generic_prec(self, n):
        return 0

    # LaTeX blocs
    def brackets(self, expr):
        """Enclose expr in {...}"""
        return r"{{{0}}}".format(expr)

    def group(self, expr):
        """Returns expr, add brackets if needed"""
        # Note: No brackets required when in parenthesis
        if len(expr) == 1 or expr.startswith(r"\left(") and expr.endswith(r"\right)"):
            return expr
        else:
            return self.brackets(expr)

    def parenthesis(self, expr):
        return r"\left({0}\right)".format(expr)

    def power(self, expr, power):
        return r"{0}^{1}".format(self.group(expr), self.group(power))

    def division(self, numerator, denominator):
        return r"\frac{0}{1}".format(self.brackets(numerator), self.brackets(denominator))

    def sqrt(self, args):
        return r"\sqrt{0}".format(self.brackets(args))

    def operator(self, func, args=None):
        if args is None:
            return r"\operatorname{{{0}}}".format(func)
        else:
            return r"\operatorname{{{0}}}{1}".format(func, self.parenthesis(args))

def TeX(str_eval, **kwargs):
    match str_eval:
        case sympy.Expr() | sympy.Basic():
            return sympy.latex(str_eval)
        case str(x) if is_operator(x):
            return Mathematical_operator(x, mode="TeX", **kwargs)
        case str():
            simplify_multipliers = True
            match Eval(str_eval):
                case _  as num if is_number(num):
                    simplify_multipliers = None
                case sympy.Number():
                    simplify_multipliers = None
                

            str_eval = LatexVisitor(simplify_multipliers=simplify_multipliers, **kwargs).visit(to_ast(str_eval))
            return str(str_eval)
        case list():
            i = 0
            tex_str = ""
            while i < len(str_eval):
                match str_eval[i]:
                    case list() as prec:
                        tex_str += TeX(prec, parentheses=True)
                    case "**" as op:
                        tex_str += f' {repr(op)}{{{TeX(str_eval[i+1], parentheses=True)}}} '
                    case _ as op if is_operator(op):
                        tex_str += f' {repr(op)} '
                    case _:
                        tex_str += TeX(str_eval[i])
                i += 1
            if kwargs.get("parentheses") == True:
                return f'\\left({tex_str}\\right)'
            return tex_str
        case _ if is_number(str_eval):
            return TeX(str(str_eval), **kwargs)
        case _:
            match type(str_eval).__name__:
                case "RandUnit":
                    return repr(str_eval)     
            return TeX(str(str_eval))

if "__main__" == __name__:
    Express = "2 - (3 + 4)"
    parse = ast.parse(Express)
    print(ast.unparse(parse))
    # print(TeX(Express))
    # Eq1 = "3*x + y == 2"
    # Eq2 = "x - y == 2"
    # Eq1 = Str2Eval(Eq1)
    # Eq2 = Str2Eval(Eq2)
    # print(Eq1, Eq2)
    # print(repr(expr))
    # print(TeX(Eq1), TeX(Eq2))
    # print(Eval(Eq1, mode="solve", eq=Eq2, symbols=("x", "y")))
    
    
    