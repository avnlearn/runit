#!/usr/bin/env python3
import ast
import importlib
import math
import cmath
import fractions
import decimal


try:
    from .ast_utils import is_ast, from_ast, to_ast
    from .extra_math import *
except ImportError:
    from ast_utils import is_ast, from_ast, to_ast
    from extra_math import *

Sympy_abc = None
try:
    import sympy
    import sympy.abc as Sympy_abc
except Exception:
    import math as sympy


def NumberName(num, **kwargs):
    match num:
        case int() | sympy.Integer():
            return "Z"
        case float() | sympy.Float() | decimal.Decimal() | fractions.Decimal():
            return "D"
        case complex() | sympy.I:
            return "C"
        case fractions.Fraction() | sympy.Rational():
            return "Q"
        case ast.Call():
            match num.func.id if isinstance(num.func, ast.Name) else num.func.attr:
                case "Integer":
                    return "Z"
                case "Decimal":
                    return "D"
                case "Fraction" | "Rational":
                    return "Q"
                case "sqrt" | "isqrt" | "root":
                    return "T"
                case "pow" | "Pow":
                    return "P"
                case "exp":
                    return "E"
                case "Symbol":
                    return "S"
                case _:
                    return "Function"
        case ast.Name() | ast.Attribute():
            match num.id if isinstance(num, ast.Name) else num.attr:
                case "I":
                    return "C"
                case "e" | "pi":
                    return "T"
                case "NaN" | "nan":
                    return "nan"
                case "inf":
                    return "inf"
                case _:
                    return "S"
        case ast.UnaryOp():
            return NumberName(num.operand)
        case ast.Constant():
            return NumberName(num.value)
        case ast.BinOp():
            match [NumberName(i) for i in [num.left, num.right]]:
                case ["Z", "Z"] if isinstance(num.op, (ast.Div, ast.FloorDiv)):
                    return "Q"
                case ["Z" | "D" | "Q", "Z" | "D" | "Q"] if isinstance(num.op, ast.Pow):
                    return "P"
                case ["Z" | "D" | "Q" | "S" | "A", "S" | "A" | "T"] | [
                    "S" | "A" | "T",
                    "Z" | "D" | "Q" | "S",
                ]:
                    return "A"
                
                case _ if isinstance(num.op, ast.Pow):
                    return "A"
                case _ as x:
                    print(x)
                    return "R"
        case str():
            return NumberName(to_ast(num))
        case _:
            match type(num).__name__:
                case "RandUnit" | "Eval":
                    return NumberName(str(num))


def is_Operator(op, **kwargs):
    match op:
        case "==" | "!=" | ">" | "<" | "<=" | ">=":
            return True
        case "and" | "or" | "not" | "is" | "is not" | "in" | "not in":
            return True
        case _ if precedence_arithmetic(op, BinOp=False):
            return True
    return False


def precedence_arithmetic(op, **kwargs):
    match op:
        case "|" | ast.BitOr():
            return 1
        case "^" | ast.BitXor():
            return 2
        case "&" | ast.BitAnd():
            return 3
        case "~" | ast.Invert():
            return 9
        case ">>" | ast.RShift():
            return 10
        case "<<" | ast.LShift():
            return 11
        case "+" | ast.Add():
            return 12
        case "-" | ast.Sub():
            return 13
        case "*" | ast.Mult() | r"\times":
            return 14
        case "@" | ast.MatMult() | r"\times":
            return 15
        case "/" | ast.Div() | r"\div":
            return 16
        case "//" | ast.FloorDiv() | r"\div":
            return 17
        case "%" | ast.Mod() | r"\mod":
            return 18
        case ast.UnaryOp():
            return 19 if isinstance(op.op, ast.UAdd) else 20
        case "**" | ast.Pow() | "^":
            return 20
        case ast.BinOp():
            match kwargs.get("BinOp"):
                case True:
                    return precedence_arithmetic(op.op, **kwargs)
                case None:
                    return 21
        case str() if kwargs.get("str"):
            return precedence_arithmetic(to_ast(op), **kwargs)

    return 0


class Eval(ast.NodeTransformer):
    def __init__(self, expression=None, *args, **kwargs):
        super(Eval, self).__init__()
        self.expression = expression
        if kwargs:
            self.__dict__.update(kwargs)

        self.expression_ast = self.Visitor(self.expression)

    def Visitor(self, expr, **kwargs):
        match expr:
            case ast.AST():
                return self.visit(expr)
            case str() if not is_Operator(expr) and expr:
                return self.Visitor(ast.parse(expr, mode="eval").body)
            case list() if expr:
                i = 0
                arr2str = ""
                while i < len(expr):
                    match expr[i]:
                        case "+" | "-" | "*" | "/" as op:
                            arr2str += f" {op} "
                            match NumberName(expr[i + 1]):
                                case "Q" if op == "/":
                                    arr2str += f"({expr[i + 1]})"
                                    i += 1
                        case list():

                            arr2str += f"({self.__class__(expr[i])})"
                        case _:
                            arr2str += f"{expr[i]}"
                    i += 1
                return self.Visitor(arr2str)
            case _:
                match type(expr).__name__:
                    case "RandUnit":
                        return self.Visitor(expr.number)
                    case "RandOperation":
                        return self.Visitor(list(expr))
                    case (
                        "Eval"
                        | "Fraction"
                        | "Decimal"
                        | "int"
                        | "float"
                        | "complex"
                        | "bool"
                    ):
                        return self.Visitor(str(expr))

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

    def __str__(self):
        expr_str = self.eval(str)
        return expr_str if expr_str != None else ""

    def __repr__(self):
        eval_str = self.expression_ast
        match eval_str:
            case ast.AST():
                eval_str = ast.unparse(eval_str)
            case _ if eval_str:
                eval_str = str(eval_str)

        if eval_str:
            return eval_str
        return ""

    def __bool__(self):
        return True if str(self) else False

    def eval(self, func=None):
        eval_str = self.expression_ast
        match eval_str:
            case ast.AST():
                eval_str = ast.unparse(eval_str)
            case _ if eval_str:
                eval_str = str(eval_str)

        if isinstance(eval_str, str) and eval_str:
            eval_str = eval(eval_str)
            if type(func).__name__ != "NoneType":
                return func(eval_str)
            else:
                match eval_str:
                    case (
                        float()
                        | decimal.Decimal()
                        | fractions.Decimal()
                        | sympy.Float()
                    ):
                        return decimal.Decimal(str(eval_str))
            return eval_str

    def __int__(self):
        return self.eval(int)

    def __float__(self):
        return self.eval(float)

    def __complex__(self):
        return self.eval(complex)

    def __round__(self):
        return self.eval(round)

    def __trunc__(self):
        return self.eval(math.trunc)

    def __floor__(self):
        return self.eval(math.floor)

    def __ceil__(self):
        return self.eval(math.ceil)

    def __neg__(self):
        return -(self.eval())

    def __pos__(self):
        return +(self.eval())

    def __abs__(self):
        return self.eval(abs)

    def __invert__(self):
        return ~(self.eval())

    def _compute(self, op, y, **kwargs):
        x = self.eval()
        y = self.__class__(y).eval()
        if kwargs.get("reflected"):
            Expr_eval = f"({y}) {op} ({x})"
        else:
            Expr_eval = f"({x}) {op} ({y})"
        return self.__class__(Expr_eval).eval()

    def __add__(self, y):
        return self._compute("+", y)

    def __sub__(self, y):
        return self._compute("-", y)

    def __mul__(self, y):
        return self._compute("*", y)

    def __truediv__(self, y):
        return self._compute("/", y)

    def __floordiv__(self, y):
        return self._compute("//", y)

    def __mod__(self, y):
        return self._compute("%", y)

    def __pow__(self, x):
        return self._compute("**", y)

    def __iadd__(self, x):
        return self.__add__(x)

    def __isub__(self, y):
        return self.__sub__(y)

    def __imul__(self, y):
        return self.__mul__(y)

    def __itruediv__(self, y):
        return self.__truediv__(y)

    def __ifloordiv__(self, y):
        return self.__floordiv__(y)

    def __imod__(self, y):
        return self.__mod__(y)

    def __ipow__(self, y):
        return self.__pow__(y)

    def __radd__(self, x):
        return self._compute("+", y, reflected=True)

    def __rsub__(self, x):
        return self._compute("-", y, reflected=True)

    def __rmul__(self, x):
        return self._compute("*", y, reflected=True)

    def __rtruediv__(self, x):
        return self._compute("/", y, reflected=True)

    def __rfloordiv__(self, x):
        return self._compute("//", y, reflected=True)

    def __rmod__(self, x):
        return self._compute("%", y, reflected=True)

    def __rpow__(self, x):
        return self._compute("**", y, reflected=True)

    def __lt__(self, x):
        return (self.eval()) < (self.__class__(x).eval())

    def __le__(self, x):
        return (self.eval()) <= (self.__class__(x).eval())

    def __gt__(self, x):
        return (self.eval()) > (self.__class__(x).eval())

    def __ge__(self, x):
        return (self.eval()) >= (self.__class__(x).eval())

    def __eq__(self, x):
        return (self.eval()) == (self.__class__(x).eval())

    def __ne__(self, x):
        return (self.eval()) != (self.__class__(x).eval())

    def to_TeX(self, **kwargs):
        return TeX(
            self.__class__(self.expression).__repr__(), floordiv=self.floordiv, **kwargs
        )

    def ModuleName(cls, func_name):
        if func_name is None:
            return
        if hasattr(sympy, func_name):
            return "sympy"
        elif hasattr(math, func_name):
            return "math"
        elif hasattr(cmath, func_name):
            return "cmath"
        elif hasattr(decimal, func_name):
            return "decimal"
        elif hasattr(fractions, func_name):
            return "fractions"

    def visit_Compare(self, node):
        node.left = self.visit(node.left)
        node.comparators = list(map(self.visit, node.comparators))
        return node

    def visit_Constant(self, node):
        """
        self.math_mode:
            "sympy" :
                int : Integer
                float : Float
                complex : I
            "decimal":
                float : Decimal
        """
        if self.number_mode:
            new_call = ast.Call(
                func=ast.Attribute(
                    value=ast.Name(id="math", ctx=ast.Load()),
                    attr="NaN",
                    ctx=ast.Load(),
                ),
                args=[node],
                keywords=[],
            )
        match self.number_mode:
            case "sympy":
                new_call.func.value.id = self.number_mode
                match NumberName(node.value):
                    case "Z":
                        new_call.func.attr = "Integer"
                        new_call.args = [ast.Constant(f"{node.value}")]
                        return new_call
                    case "D":
                        new_call.attr = "Float"
                        new_call.args = [ast.Constant(f"{node.value}")]
                        return new_call
            case "decimal" | "fractions" | "fraction" | "rational" if NumberName(
                node.value
            ) in (
                "Z",
                "D",
            ):
                new_call.func.value.id = {
                    "fraction": "fractions",
                    "rational": "sympy",
                }.get(self.number_mode, self.number_mode)
                new_call.func.attr = {
                    "fraction": "Fraction",
                    "rational": "Rational",
                }.get(self.number_mode, "Decimal")
                new_call.args = [ast.Constant(f"{node.value}")]
                return new_call

        return node

    def visit_Call(self, node):

        func_name = None
        node.args = list(map(self.visit, node.args))
        match node.func:
            case ast.Name():
                func_name = node.func.id
            case ast.Attribute() if self.attribute_change and isinstance(
                node.func.value, ast.Name
            ):
                func_name = node.func.attr

        new_node = ast.Call(
            func=ast.Attribute(
                value=ast.Name(id="math", ctx=ast.Load()), attr="NaN", ctx=ast.Load()
            ),
            args=node.args,
            keywords=node.keywords,
        )

        if not self.ModuleName(func_name):
            return node

        match func_name:
            case "Fraction" if self.rational_method and self.ModuleName("Rational"):
                new_node.func.value.id = "sympy"
                new_call.func.attr = "Rational"
                return new_node
            case "Fraction":
                new_node.func.value.id = "fractions"
                new_node.func.attr = func_name
                return new_node
            case "Decimal" if len(new_node.args) and self.decimal_func_str != False:
                new_node.func.value.id = "decimal"
                new_node.func.attr = func_name
                new_node.args[0] = ast.Call(
                    func=ast.Name(id="str", ctx=ast.Load()),
                    args=[new_node.args[0]],
                    keywords=[],
                )
                return new_node
            case str(func_name) if (module_name := self.ModuleName(func_name)):
                new_node.func.value.id = module_name
                new_node.func.attr = func_name
                return new_node

        return node

    def visit_UnaryOp(self, node):
        node.operand = self.visit(node.operand)
        return node

    def visit_Name(self, node):
        new_attr = ast.Attribute(
            value=ast.Name(id="math", ctx=ast.Load()), attr="NaN", ctx=ast.Load()
        )
        # new_call = ast.Call(
        #     func=,
        #     args=node.args,
        #     keywords=node.keywords,
        # )
        match node.id:
            case "e" | "E" | "pi" | "I" | "NaN" | "nan" | "inf" if (
                module_name := self.ModuleName(node.id)
            ):
                new_attr.value.id = module_name
                new_attr.attr = node.id
                return new_attr
            case _ if hasattr(Sympy_abc, node.id):
                new_attr.value.id = "sympy"
                new_attr.attr = "Symbol"
                return ast.Call(
                    func=new_attr,
                    args=[ast.Constant(value=f"{node.id}")],
                    keywords=[],
                )
        return node

    def visit_BinOp(self, node):
        """
        self.compound_fraction :
            True :
                1/2/3/... => Fraction(1, Fraction(2, Fraction(3, ...)))
                1/2/3/... => Rational(1, Rational(2, Rational(3, ...)))
        self.div_fraction:
            Any     : 1/2/3/...     => Fraction(1, 2) / Fraction(3, ...)
            False   : 1/2/3/...     => 1/2/3/...

        self.eval_BinOp:
            Any     : (1 + 2)/2 + (3 + 4)/(7 * 3)   =>  (1 + 2)/2 + (3 + 4)/(7 * 3)
            True    : (1 + 2)/2 + (3 + 4)/(7 * 3)   =>  Fraction(1 + 2, 2) + Fraction(3 + 4, 7 * 3)

        """
        args = list(map(self.visit, [node.left, node.right]))
        node.left, node.right = tuple(args)

        new_call = ast.Call(
            func=ast.Attribute(
                value=ast.Name(id="fractions", ctx=ast.Load()),
                attr="Fraction",
                ctx=ast.Load(),
            ),
            args=args,
            keywords=[],
        )
        args_eval = args
        if self.eval_BinOp == True:
            args_eval = [self.__class__(i).eval() for i in args_eval]
        args_lname_rname = list(map(NumberName, args_eval))

        match node.op:
            case ast.FloorDiv() if self.floordiv == False:
                node.op = ast.Div()

        match node.op:
            # sympy.Rational
            case ast.Div():
                if self.sympy:
                    new_call.func.value.id = "sympy"
                    new_call.func.attr = "Rational"
                    new_call.args = [
                        ast.Call(
                            func=ast.Name(id="str", ctx=ast.Load()),
                            args=[i],
                            keywords=[],
                        )
                        for i in args
                    ]

                match args_lname_rname:
                    case [
                        "Z" | "D" | "Q",
                        "Z" | "D" | "Q",
                    ] if self.compound_fraction and self.sympy:
                        return new_call
                    case ["Z" | "D", "Z" | "D"] if self.sympy:
                        return new_call
                    case ["Z" | "Q", "Z" | "Q"] if self.compound_fraction:
                        return new_call
                    case ["Z", "Z"]:
                        return new_call

        return node


class TeX(ast.NodeVisitor):
    def __init__(self, expr_var=None, *args, **kwargs):
        super(TeX, self).__init__()
        self.expr_var = expr_var

        if kwargs:
            self.__dict__.update(kwargs)

        if self.parenthesis:
            self.lbracket = 1
            self.rbracket = 1

        self.TeX_str = self.Visitor(self.expr_var)

    def Visitor(self, expr, **kwargs):
        match expr:
            case ast.AST():
                return self.visit(expr)
            case "+" | "-":
                return str(expr)
            case "*" | "\\times" | ast.Mult() | ast.MatMult():
                return r"\times"
            case "/" | "<<" | ">>" | "//":
                return "\\div"
            case "%" | ast.Mod():
                return r"\mod"
            case "**":
                return r"^"
            case str() if not is_Operator(expr) and expr:
                return self.Visitor(ast.parse(expr, mode="eval").body)
            case list() if expr:
                i = 0
                arr2str = ""
                while i < len(expr):
                    match expr[i]:
                        case "+" | "-" | "*" | "/" | "//" as op:
                            arr2str += f" {repr(op)} "
                            match (
                                NumberName(expr[i + 1])
                                if precedence_arithmetic(str(expr[i + 1])) != 0
                                else None
                            ):
                                case "Q" | "R" if op == "/":
                                    arr2str += f"\\left({repr(expr[i + 1])}\\right)"
                                    i += 1
                        case list():
                            arr2str += f"\\left({self.Visitor(expr[i])}\\right)"
                        case _:
                            match precedence_arithmetic(str(expr[i]), str=True):
                                case 20:
                                    arr2str += r"\left({}\right)".format(TeX(expr[i]))
                                case _:
                                    arr2str += f"{repr(expr[i])}"
                    i += 1
                return arr2str
            case sympy.Expr() | sympy.Basic():
                return sympy.latex(expr)
            case _:
                match type(expr).__name__:
                    case (
                        "RandUnit"
                        | "Eval"
                        | "Fraction"
                        | "Decimal"
                        | "int"
                        | "float"
                        | "complex"
                    ):
                        return self.Visitor(str(expr))
                    case "RandOperation":
                        return self.Visitor(list(expr))

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

    def __str__(self):
        match self.TeX_str:
            case ast.AST():
                return ast.unparse(self.TeX_str)
            case _ if self.TeX_str:
                return str(self.TeX_str)
        return ""

    def __repr__(self):
        match self.expr_var:
            case ast.AST():
                return ast.unparse(self.expr_var)
            case _ if self.expr_var:
                return str(self.expr_var)
        return ""

    def visit_frac(self, numerator, denominator):
        if isinstance(numerator, ast.AST):
            numerator = self.visit(numerator)
        if isinstance(denominator, ast.AST):
            denominator = self.visit(denominator)
        return r"\frac{{{0}}}{{{1}}}".format(numerator, denominator)

    def visit_exponent(self, base, exponent):
        match base:
            case ast.UnaryOp():
                base = f"\\left({self.visit(base)}\\right)"
            case ast.BinOp() if not isinstance(base.op, ast.Pow):
                base = f"\\left({self.visit(base)}\\right)"
            case ast.AST():
                base = self.visit(base)

        match exponent:
            case ast.AST():
                exponent = self.visit(exponent)

        return f"{base}^{{{exponent}}}"

    def visit_Symbol(self, node):
        match node:
            case ast.AST():
                node = self.visit(node)

        match node:
            case "lamda":
                return r"\lambda"
            case "Lamda":
                return r"\Lambda"
            case "PI":
                return r"\pi"
            case "I":
                return r"\iota"
            case str(x) if x.lower() in [
                "alpha",
                "beta",
                "chi",
                "delta",
                "epsilon",
                "eta",
                "gamma",
                "iota",
                "kappa",
                "mu",
                "Nu",
                "omega",
                "omicron",
                "phi" "pi",
                "psi",
                "rho",
                "sigma",
                "tau",
                "theta",
                "upsilon",
                "xi",
                "zeta",
            ]:
                return f"\\{node}"

        if self.text_mode:
            return f"\\text{{{node}}}"
        return node

    def visit_Attribute(self, node):
        mul = ""
        match node.value:
            case ast.Call():
                mal = ""
            case ast.Name():
                return self.visit_Symbol(node.attr)
        return mul

    def visit_Name(self, node):
        match node.id:
            case ast.AST():
                node = self.visit(node)
        return self.visit_Symbol(node.id)

    def visit_sqrt(self, base, exponent=None, *args):
        match base:
            case ast.AST():
                base = self.visit(base)

        match exponent:
            case ast.Constant():
                if exponent.value != 2:
                    exponent = self.visit(exponent)
                else:
                    exponent = None
            case ast.AST():
                exponent = self.visit(exponent)

        if exponent != None:
            exponent = f"[{exponent}]"
        else:
            exponent = ""
        return r"\sqrt{0}{{{1}}}".format(exponent, base)

    def visit_Call(self, node):

        func = None
        match node.func:
            case ast.Name():
                func = node.func.id
            case ast.Attribute() if isinstance(node.func.value, ast.Name):
                func = node.func.attr

        parameter = True if len(node.args) else False
        parameter_kwargs = True if len(node.keywords) else True

        args = list(map(self.visit, node.args))
        match func:
            case "sqrt":
                return self.visit_sqrt(args[0])
            case "Fraction" | "Rational":
                return self.visit_frac(args[0], args[1])
            case "Decimal" | "Float" | "float":
                return str(args[0])
            case "Pow" | "pow":
                return self.visit_exponent(args[0], args[1])
            case "root":
                return self.visit_sqrt(*node.args)
            case "Symbol":
                return self.visit_Symbol(args[0])
            case "str":
                return str(args[0])
        return ""

    def visit_Constant(self, node):
        return str(node.value)

    def visit_UnaryOp(self, node):
        match node.op:
            case ast.UAdd():
                return self.visit(node.operand)
            case ast.USub():
                node = self.visit(node.operand)
                return f"-{node}"

    def visit_Add(self, node):
        return "+"

    def visit_Sub(self, node):
        return "-"

    def visit_Mult(self, node):
        return r"\times"

    def visit_Div(self, node):
        return r"\div"

    def visit_FloorDiv(self, node):
        return r"\div"

    def visit_Pow(self, node):
        return r"^"

    def visit_Mod(self, node):
        return r"\mod"

    def visit_BitXor(self, node):
        return r"^"

    def pedmas(self, node):
        return precedence_arithmetic(node)

    def paren(self, value, level):
        match value:
            case ast.AST():
                value = self.visit(value)

        match level if self.parenthesis else None:
            case 1 | None:
                return f"\\left({value}\\right)"
            case 2:
                return f"\\left\\{{{value}\\right\\}}"
            case _:
                return f"\\left[{value}\\right]"

    def visit_BinOp(self, node):
        operand = list(map(self.visit, [node.left, node.op, node.right]))

        if isinstance(node.left, (ast.BinOp, ast.UnaryOp)):
            match [node.left.op, node.op]:
                case [ast.Pow(), ast.Pow()] | [ast.Sub() | ast.Add(), ast.Mult()]:
                    operand[0] = self.paren(operand[0], self.lbracket)
                    self.lbracket = (
                        self.lbracket + 1 if isinstance(self.lbracket, int) else None
                    )
                
                case _:
                    print(ast.dump(node.left.op), ast.dump(node.op))

        if isinstance(node.right, (ast.BinOp, ast.UnaryOp)):
            match [node.op, node.right.op]:
                case (
                    [ast.Div(), ast.Div()]
                    | [ast.Sub(), ast.Sub()]
                    | [ast.Pow(), ast.Pow()]
                ):
                    operand[2] = self.paren(operand[2], self.rbracket)
                    self.rbracket = (
                        self.rbracket + 1 if isinstance(self.rbracket, int) else None
                    )
                case _ if self.pedmas(node.op) > self.pedmas(node.right.op):
                    operand[2] = self.paren(operand[2], self.rbracket)
                    self.rbracket = (
                        self.rbracket + 1 if isinstance(self.rbracket, int) else None
                    )

        match node.op:
            case ast.Div() if self.div != False:
                return self.visit_frac(node.left, node.right)
            case ast.FloorDiv() if self.floordiv != False:
                return r"\lfloor{}\rfloor".format(
                    self.visit_frac(node.left, node.right)
                )
            case ast.Pow():
                return self.visit_exponent(node.left, node.right)
            case ast.Mult():
                match [NumberName(i) for i in [node.left, node.right]]:
                    case ["Z" | "D" | "Q" | "T" | "S" | "A", "T" | "S" | "A"] | [
                        "T" | "S" | "A",
                        "Z" | "D" | "Q" | "T" | "S" | "A",
                    ]:
                        operand[1] = ""
                    case [lname, rname]:
                        # pass
                        print(lname, rname)

            case ast.Mult() if self.algebra:
                if all(
                    isinstance(i, (ast.Name, ast.Attribute))
                    for i in [node.left, node.right]
                ):
                    operand[1] = ""
                elif isinstance(node.left, ast.BinOp):
                    if self.pedmas(node.op) > self.pedmas(node.left):
                        operand[1] = ""
                elif isinstance(node.right, ast.BinOp):
                    if self.pedmas(node.op) > self.pedmas(node.right):
                        operand[1] = ""

                match [NumberName(node.left), NumberName(node.right)]:
                    case ["T" | "C" | "A" | "S", "Z" | "D" | "Q"] | [
                        "Z" | "D" | "Q",
                        "T" | "C" | "A" | "S",
                    ]:
                        operand[0], operand[2] = operand[2], operand[0]
                        operand[1] = ""
                    case ["S" | "A", "S" | "A"]:
                        operand[1] = ""
                    case ["C", "Z" | "D" | "Q" | "T" | "R" | "C" | "A" | "S" | "A"] | [
                        "Z" | "D" | "Q" | "T" | "R" | "C" | "A" | "S" | "A",
                        "C",
                    ]:
                        operand[1] = ""

        return " ".join(operand)


if "__main__" == __name__:
    a = TeX("(a - 2)*(a + 2)")
    print(repr(a))
    print(a)
