import ast
import sys
import random
import re as py_re
import sympy
from randunit_ast.ast_math_expr import *


sys.setrecursionlimit(100000000)


class RandUnit:
    def __init__(self, unit=None, name=None, *args, **kwargs):
        self.unit = unit
        self.name = name
        if args:
            self.args = args

        if kwargs:
            self.kwargs = kwargs
            self.__dict__.update(self.kwargs)
        else:
            self.kwargs = {}

        if self.number is not None:
            self.unit = self.number

        if not isinstance(self.eval_BinOp, bool):
            self.eval_BinOp = True

        if isinstance(self.unit, list):
            self.unit = random.choice(self.unit)
            self.__dict__.update(self.__class__(**self.__dict__).__dict__)
        #   self.__class__()
        elif unit := Eval(self.unit, eval_BinOp=self.eval_BinOp):
            self.unit = repr(unit)
            self.name = self.NumberName(self.unit, self.name)
            self.number = (
                self.RUnit(self.unit, self.name, self.zero)
                if self.number is None
                else self.unit
            )
        else:
            self.number = self.unit
            self.name = False

    def __str__(self):
        num = from_ast(self)
        return str(num) if num else ""

    def __repr__(self):
        num = TeX(self)
        return str(num) if num else ""

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

    def __bool__(self):
        return False if self.name == False else True

    def NumberName(self, number, name=None, **kwargs):
        if isinstance(name, (str, list, set, tuple)):
            name_lst = [
                "Z",
                "F",
                "D",
                "Q",
                "QD",
                "T",
                "P",
                "R",
                "Imag",
                "C",
                "S",
                "A",
                "Function",
                "Equation",
                "Set",
                "Coordinate",
                "List",
            ]
            match name:
                case "N":
                    return self.NumberName(number, "Z+", **kwargs)
                case "F":
                    return self.NumberName(number, "Q+", **kwargs)
                case str() if name.upper() in name_lst:
                    return name.upper() + random.choice("+-")
                case str() if (
                    n := name.upper().replace("+", "").replace("-", "")
                ) in name_lst:
                    if kwargs.get("index"):
                        return name_lst.index(n)
                    return name.upper()
                case list() if name:
                    return self.NumberName(number, random.choice(name), **kwargs)
                case tuple() | set():
                    return self.NumberName(number, list(name), **kwargs)
                case _ if number:
                    return self.NumberName(number, None, **kwargs)
                case _ if kwargs.get("index"):
                    return 0

        elif num := Eval(number, eval_BinOp=self.eval_BinOp):
            return self.ast_expr_dict(repr(num), name=True)
        elif is_Operator(number):
            return False

    def ast_expr_dict(self, node, **kwargs):
        number_args = {"unit": node}
        match node:
            case int() | float():
                return self.ast_expr_dict(str(node), **kwargs)
            case complex():
                number_args.update({"name": "Imag+"})
            case str() if node.isdecimal():
                number_args.update({"name": "Z+", "unit": int(node)})
            case str() if node.replace(".", "").isdecimal():
                matches = py_re.search(
                    r"^(?P<Integer_Part>\d+)?\.(?P<Zero_Part>0+)?(?P<Decimal_Part>\d+)$",
                    node,
                )
                number_args.update(matches.groupdict())
                number_args.update({"name": "D+", "unit": float(node)})
            case str():
                return self.ast_expr_dict(to_ast(node), **kwargs)
            case ast.Constant():
                return self.ast_expr_dict(node.value, **kwargs)
            case ast.UnaryOp():
                name = self.NumberName(node.operand)
                match name:
                    case "Z+" | "D+" if isinstance(node.operand, ast.Constant):
                        node = self.ast_expr_dict(node.operand)
                        number_args.update(node)
                    case str():
                        number_args.update({"name": name.replace("+", "-")})
                        node = node.operand
                        number_args.update({"unit": node})

            case ast.Name():
                match str(node.id).lower():
                    case "e" | "pi":
                        number_args.update({"name": "T+"})
                    case _:
                        number_args.update({"name": "S+"})
            case ast.Attribute() if isinstance(node.value, ast.Name):
                match node.attr:
                    case "e" | "pi":
                        number_args.update({"name": "T+"})
                    case _:
                        number_args.update({"name": "Attr+"})
            case ast.Attribute():
                number_args.update({"name": "Attr+"})
            case ast.BinOp():
                match node.op:
                    case ast.Pow():
                        number_args.update({"name": "P+"})
                    case _:
                        lname = self.NumberName(node.left)
                        rname = self.NumberName(node.right)
                        if any(name in ("Imag+", "Imag-") for name in [lname, rname]):
                            number_args.update({"name": "C+"})
                        elif any(
                            name in ("S+", "Function+", "S-", "Function-")
                            for name in [lname, rname]
                        ):
                            number_args.update({"name": "A+"})
                        else:
                            number_args.update({"name": "R+"})
            case ast.Compare():
                number_args.update({"name": "Equation+"})
            case ast.Set():
                number_args.update({"name": "Set+"})
            case ast.Tuple():
                number_args.update({"name": "Coordinate+"})
            case ast.List():
                number_args.update({"name": "List+"})
            case ast.Call():
                func_name = None
                match node.func:
                    case ast.Name():
                        func_name = node.func.id
                    case ast.Attribute():
                        func_name = node.func.attr
                match func_name:
                    case "sqrt" | "cbrt" if len(node.args) > 0:
                        number_args.update({"name": "T+"})
                    case "root" if len(node.args) >= 2:
                        number_args.update({"name": "T+"})
                    case "pow" | "Pow" if len(node.args) > 0:
                        number_args.update({"name": "P+"})
                    case "Symbol" | "symbols":
                        number_args.update({"name": "S+"})
                    case "Decimal" | "Float" | "float":
                        number_args.update({"name": "D+"})
                    case "integer" if len(node.args) == 1:
                        number_args.update(
                            {"name": "Int", "unit": node.args[0]})
                    case "Fraction" | "FractionSlash" if len(node.args) >= 2:
                        p = node.args[0]
                        q = node.args[1]
                        p_name = self.NumberName(p)
                        q_name = self.NumberName(q)
                        number_args.update(
                            {"numerator": None, "denominator": None})
                        if is_ast(self.numerator):
                            p_name = self.NumberName(self.numberator)
                            p = to_ast(self.numberator)
                            number_args.update({"numerator": True})
                        elif isinstance(self.numberator, bool):
                            number_args.update({"numerator": self.numberator})

                        if is_ast(self.denominator):
                            p_name = self.NumberName(self.denominator)
                            q = to_ast(self.denominator)
                            number_args.update({"denominator": True})
                        elif isinstance(self.denominator, bool):
                            number_args.update(
                                {"denominator": self.denominator})

                        number_args.update({"neg_p": "", "neg_q": ""})
                        if isinstance(p, ast.UnaryOp):
                            if isinstance(p.op, ast.USub):
                                number_args.update({"neg_p": "-"})
                            p = p.operand

                        if isinstance(q, ast.UnaryOp):
                            if isinstance(q.op, ast.UnaryOp):
                                if isinstance(q.op, ast.USub):
                                    number_args.update({"neg_q": "-"})
                            q = q.operand

                        unit_p, unit_q = from_ast(p), from_ast(q)
                        if unit_p == unit_q:
                            if (
                                not isinstance(self.proper_fraction, bool)
                                and self.divisible != True
                            ):
                                self.proper_fraction = random.choice(
                                    [True, False])
                            elif self.divisible == True:
                                self.proper_fraction = False
                            number_args.update(
                                {"equal": True, "proper_fraction": self.proper_fraction}
                            )
                        else:
                            self.proper_fraction = True if unit_p < unit_q else False
                            number_args.update(
                                {"proper_fraction": self.proper_fraction}
                            )
                        number_args.update({"name": "Q+"})
                        if any(number_args.get(i) for i in ["neg_p", "neg_q"]):
                            number_args.update({"name": "Q-"})
                        node.args[0] = p
                        node.args[1] = q
                        number_args.update({"unit": node})
                    case "FixUnit" if node.args:
                        number_args.update(
                            {"name": "Fix+", "unit": node.args[0]})
                    case _:
                        number_args.update({"name": "Function+"})
            case _ if isinstance(node, ast.AST):
                print(ast.dump(node, indent=4))

        return number_args.get("name") if kwargs.get("name") else number_args

    def Integer_Positive(self, unit, zero=None, **kwargs):
        unit = unit if isinstance(
            unit, dict) and unit else self.ast_expr_dict(unit)

        match unit.get("name") if isinstance(unit, dict) else unit:
            case "Z+" | "Z-" if isinstance(unit.get("unit"), (str, int)):
                number = int(unit["unit"])
                if py_re.search(r"^(?P<Number>[1-9]+)(?P<Zero>0+)$", str(unit["unit"])):
                    matches = py_re.search(
                        r"^(?P<Number>[1-9]+)(?P<Zero>0+)$", str(unit["unit"])
                    )
                    number = int(
                        "{}{}".format(
                            from_ast(self.RUnit(
                                matches.group("Number"), "Z+", zero)),
                            matches.group("Zero"),
                        )
                    )
                    return ast.Constant(value=number)
                elif number <= 0:
                    return ast.Constant(value=None)
                elif number == 1:
                    match zero:
                        case True:
                            start = 0
                        case False:
                            start = 2
                        case None:
                            start = 1
                        case int():
                            start = zero
                elif isinstance(self.start_unit, int):
                    start = self.start_unit
                else:
                    start = 10 ** (number - 1)

                stop = 0
                if isinstance(self.stop_unit, int):
                    stop = self.stop_unit

                if stop <= start:
                    stop = (10**number) - 1
                else:
                    number = None

                if (
                    self.rand_unit != False
                    and isinstance(start, int)
                    and isinstance(stop, int)
                ):
                    if self.prime:
                        number = sympy.randprime(start, stop)
                    else:
                        number = random.randrange(start, stop)

                return ast.Constant(value=number)
            case "Int":
                name = self.NumberName(None, "Z")
                return self.RUnit(unit.get("unit"), name, zero=zero, **kwargs)
            case str() as name:
                self.name = name
                return self.RUnit(unit, name, zero=zero, **kwargs)
            case _:
                return ast.Constant(value=None)

    def Decimal_Positive(self, unit, zero, **kwargs):
        unit = unit if isinstance(
            unit, dict) and unit else self.ast_expr_dict(unit)

        match unit.get("name") if isinstance(unit, dict) else unit:
            case "D+" | "D-" if unit.get("Integer_Part") and unit.get("Decimal_Part"):
                Integer_Part = self.RUnit(
                    unit["Integer_Part"], "Z+", True, **kwargs)
                Integer_Part = from_ast(Integer_Part)
                Decimal_Part = self.RUnit(
                    unit["Decimal_Part"], "Z+", None, **kwargs)
                Decimal_Part = from_ast(Decimal_Part)
                Zero_Part = (
                    unit["Zero_Part"] if isinstance(
                        unit["Zero_Part"], str) else ""
                )
                number = float(f"{Integer_Part}.{Zero_Part}{Decimal_Part}")
                return ast.Constant(value=number)
            case "D+" | "D-" if isinstance((node := unit.get("unit")), ast.Call):
                node.args = [self.RUnit(i, zero=zero, **kwargs)
                             for i in node.args]
                return node
            case str() as name:
                self.name = name
                return self.RUnit(unit, name, zero=zero, **kwargs)
            case _:
                return ast.Constant(value=None)

    def Rational_Proper(self, p, q):
        unit_p = from_ast(self.RUnit(p, zero=None))
        unit_q = from_ast(self.RUnit(q, zero=False))
        if unit_p < unit_q and self.proper_fraction == True:
            return (ast.Constant(value=unit_p), ast.Constant(value=unit_q))
        elif unit_p > unit_q and self.proper_fraction == False:
            if self.divisible:
                if unit_p % unit_q == 0:
                    return (ast.Constant(value=unit_p), ast.Constant(value=unit_q))
                else:
                    return self.Rational_Proper(p, q)
            else:
                return (ast.Constant(value=unit_p), ast.Constant(value=unit_q))

        return self.Rational_Proper(p, q)

    def Rational_Positive(self, unit, zero, **kwargs):
        unit = unit if isinstance(
            unit, dict) and unit else self.ast_expr_dict(unit)

        match unit.get("name") if isinstance(unit, dict) else unit:
            case "Q+" | "Q-":
                node = unit.get("unit")
                p = q = None
                default = False
                match node:
                    case ast.BinOp():
                        p = node.left
                        q = node.right
                    case ast.Call() if len(node.args) >= 2:
                        default = True
                        p = node.args[0]
                        q = node.args[1]
                    case _:
                        return ast.Constant(value=None)

                if all(unit.get(i) == None for i in ["numerator", "denominator"]):
                    p, q = self.Rational_Proper(p, q)
                else:
                    if unit.get("numerator") == None:
                        p = self.RUnit(p, zero=None)

                    if unit.get("denominator") == None:
                        q = self.RUnit(q, zero=False)

                if isinstance(node, ast.Call):
                    node.args[0] = p
                    node.args[1] = q

                    return node
                else:
                    if self.sympy:
                        return ast.Call(
                            func=ast.Name(id="Rational", ctx=ast.Load()),
                            args=[p, q],
                            keywords=[],
                        )
                    return ast.Call(
                        func=ast.Name(id="Fraction", ctx=ast.Load()),
                        args=[p, q],
                        keywords=[],
                    )
            case str() as name:
                self.name = name
                return self.RUnit(unit, name, zero=zero, **kwargs)
            case _:
                return ast.Constant(value=None)

    def Irrational_Positive(self, unit, zero, **kwargs):
        unit = unit if isinstance(
            unit, dict) and unit else self.ast_expr_dict(unit)

        match unit.get("name") if isinstance(unit, dict) else unit:
            case "T+" | "T-" if self.root_change == False:
                return unit.get("unit")
            case "T+" | "T-":
                node = unit.get("unit")
                match node:
                    case ast.Call():
                        node.args[0] = self.RUnit(
                            node.args[0], zero=zero, **kwargs)
                        type_func = None
                        match node.func:
                            case ast.Name():
                                type_func = node.func.id
                            case ast.Attribute():
                                type_func = node.func.attr
                                if (
                                    isinstance(node.func.value, ast.Call)
                                    and self.attribute_change != False
                                ):
                                    node.func.value = self.RUnit(
                                        node.func.value, zero=zero, **kwargs
                                    )

                        if type_func == "root" and self.root_change != True:
                            node.args[1] = self.RUnit(
                                node.args[1], zero=False, **kwargs
                            )

                        if self.perfect_root == True:
                            name = self.NumberName(node.args[0])
                            match type_func:
                                case "sqrt" | "isqrt":
                                    match name:
                                        case "Z+" | "D+" | "Q+":
                                            args = self.__class__(
                                                number=str(
                                                    Eval(
                                                        node.args[0], sympy=True) ** 2
                                                )
                                            ).__str__()
                                            node.args[0] = to_ast(args)
                                        case "Z-" | "D-" | "Q-":
                                            args = self.__class__(
                                                number=str(
                                                    Eval(
                                                        node.args[0], sympy=True) ** 2
                                                )
                                            ).__str__()
                                            node.args[0] = to_ast(f"-{args}")
                                        case _:
                                            node.args[0] = to_ast(
                                                f"({from_ast(
                                                    node.args[0])})**2"
                                            )

                                case "cbrt":
                                    match name:
                                        case "Z+" | "D+" | "Z-" | "D-" | "Q+" | "Q-":
                                            args = self.__class__(
                                                number=str(
                                                    Eval(
                                                        node.args[0], sympy=True) ** 3
                                                )
                                            ).__str__()
                                            node.args[0] = to_ast(args)
                                        case _:
                                            node.args[0] = to_ast(
                                                f"({from_ast(
                                                    node.args[0])})**3"
                                            )
                                case "root":
                                    name_exponent = self.NumberName(
                                        node.args[1])
                                    num_exponent = Eval(
                                        node.args[1], sympy=True)
                                    odd_number = True
                                    if num_exponent % 2 == 0:
                                        odd_number = False
                                    if name_exponent in ("Z+", "D+", "Z-", "D-"):
                                        match name:
                                            case (
                                                "Z+" | "D+" | "Z-" | "D-" | "Q+" | "Q-"
                                            ) if odd_number:
                                                args = self.__class__(
                                                    number=str(
                                                        Eval(
                                                            node.args[0], sympy=True)
                                                        ** num_exponent
                                                    )
                                                ).__str__()
                                                node.args[0] = to_ast(args)
                                            case "Z+" | "D+":
                                                args = self.__class__(
                                                    number=str(
                                                        Eval(
                                                            node.args[0], sympy=True)
                                                        ** num_exponent
                                                    )
                                                ).__str__()
                                                node.args[0] = to_ast(args)

                                            case "Z-" | "D-":
                                                args = self.__class__(
                                                    number=str(
                                                        Eval(
                                                            node.args[0], sympy=True)
                                                        ** num_exponent
                                                    )
                                                ).__str__()
                                                node.args[0] = to_ast(
                                                    f"-{args}")
                                            case _:
                                                node.args[0] = to_ast(
                                                    f"({from_ast(
                                                        node.args[0])})**{num_exponent}"
                                                )
                    case ast.Attribute() if isinstance(node.value, ast.Name):
                        return node
                    case ast.Attribute() if isinstance(node.value, ast.Attribute):
                        node.value = self.RUnit(
                            node.value, zero=zero, **kwargs)

                return node
            case str() as name:
                self.name = name
                return self.RUnit(unit, name, zero=zero, **kwargs)
            case _:
                return ast.Constant(value=None)

    def Imaginary_Positive(self, unit, zero, **kwargs):
        unit = unit if isinstance(
            unit, dict) and unit else self.ast_expr_dict(unit)
        match unit.get("name") if isinstance(unit, dict) else unit:
            case "Imag+" | "Imag-":
                node = unit["unit"]
                imag = self.RUnit(node.imag, zero=zero, **kwargs)
                match self.name:
                    case "R+" | "R-":
                        self.name = self.name.replace("R", "C")
                return ast.Constant(value=complex(0, from_ast(imag)))
            case str() as name:
                self.name = name
                return self.RUnit(unit, name, zero=zero, **kwargs)
            case _:
                return ast.Constant(value=None)

    def Symbol_Positive(self, unit, zero, **kwargs):
        unit = unit if isinstance(
            unit, dict) and unit else self.ast_expr_dict(unit)

        match unit.get("name") if isinstance(unit, dict) else unit:
            case "S+" | "S-":

                return unit.get("unit", ast.Constant(value=None))
            case str() as name:
                self.name = name
                return self.RUnit(unit, name, zero=zero, **kwargs)
            case _:
                return ast.Constant(value=None)

    def Real_Positive(self, unit, zero, **kwargs):
        unit = unit if isinstance(
            unit, dict) and unit else self.ast_expr_dict(unit)

        match unit.get("name") if isinstance(unit, dict) else unit:
            case "R+" | "R-" | "A+" | "A-" | "C+" | "C-" if isinstance(
                (node := unit.get("unit")), ast.BinOp
            ):
                node.left = self.RUnit(node.left, zero=zero, **kwargs)
                node.right = self.RUnit(node.right, zero=zero, **kwargs)
                return node
            case str() as name:
                self.name = name
                return self.RUnit(unit, name, zero=zero, **kwargs)
            case _:
                return ast.Constant(value=None)

    def Power_Positive(self, unit, zero, **kwargs):
        unit = unit if isinstance(
            unit, dict) and unit else self.ast_expr_dict(unit)

        match unit.get("name") if isinstance(unit, dict) else unit:
            case "P+" | "P-":
                node = unit.get("unit")
                match node:
                    case ast.BinOp():
                        if self.base_change != False:
                            node.left = self.RUnit(
                                node.left, zero=zero, **kwargs)
                        if self.power_change != False:
                            node.right = self.RUnit(
                                node.right, zero=zero, **kwargs)
                    case ast.Call() if len(node.args) > 0:
                        if self.base_change != False:
                            node.args[0] = self.RUnit(
                                node.args[0], zero=zero, **kwargs)
                        if self.power_change != False:
                            node.args[1] = self.RUnit(
                                node.args[1], zero=zero, **kwargs)
                    case _:
                        return ast.Constant(value=None)
                return node
            case str() as name:
                self.name = name
                return self.RUnit(unit, name, zero=zero, **kwargs)
            case _:
                return ast.Constant(value=None)

    def Function_Positive(self, unit, zero, **kwargs):
        unit = unit if isinstance(
            unit, dict) and unit else self.ast_expr_dict(unit)

        match unit.get("name") if isinstance(unit, dict) else unit:
            case "Function+" | "Function-":
                node = unit.get("unit")
                match node:
                    case ast.Call():
                        if (
                            isinstance(node.func, ast.Attribute)
                            and self.attribute_change != False
                        ):
                            if isinstance(node.func.value, ast.Call):
                                node.func.value = self.RUnit(
                                    node.func.value, zero=zero, **kwargs
                                )
                        for i in range(len(node.args)):
                            node.args[i] = self.RUnit(
                                node.args[i], zero=zero, **kwargs)

                        for i in range(len(node.keywords)):
                            node.keywords[i] = self.RUnit(
                                node.keywords[i], zero=zero, **kwargs
                            )
                return node
            case str() as name:
                self.name = name
                return self.RUnit(unit, name, zero=zero, **kwargs)
            case _:
                return ast.Constant(value=None)

    def Equation_Positive(self, unit, zero, **kwargs):
        unit = unit if isinstance(
            unit, dict) and unit else self.ast_expr_dict(unit)

        match unit.get("name") if isinstance(unit, dict) else unit:
            case "Equation+" | "Equation-":
                node = unit["unit"]
                node.left = self.RUnit(node.left, zero=zero, **kwargs)
                node.comparators = [
                    self.RUnit(i, zero=zero, **kwargs) for i in node.comparators
                ]
                return node
            case str() as name:
                self.name = name
                return self.RUnit(unit, name, zero=zero, **kwargs)
            case _:
                return ast.Constant(value=None)

    def Set_Positive(self, unit, zero, **kwargs):
        unit = unit if isinstance(
            unit, dict) and unit else self.ast_expr_dict(unit)
        match unit.get("name") if isinstance(unit, dict) else unit:
            case "Set+" | "Set-" if (node := unit.get("unit"), ast.Set):
                i = 0
                while i < len(node.elts):
                    val_set = self.RUnit(node.elts[i])
                    if val_set not in node.elts:
                        node.elts[i] = val_set
                        i += 1
                return node
            case "Coordinate+" | "Coordinate-" | "List+" | "List-" if (
                node := unit.get("unit"),
                (ast.Tuple, ast.List),
            ):
                node.elts = [self.RUnit(i, zero=zero, **kwargs)
                             for i in node.elts]
                return node
            case str() as name:
                self.name = name
                return self.RUnit(unit, name, zero=zero, **kwargs)
            case _:
                return ast.Constant(value=None)

    def RUnit(self, unit, name=None, zero=None, **kwargs):
        match unit:
            case None:
                return ""
            case "0" | 0:
                return ast.Constant(value=0)

        if name is None:
            match unit:
                case str() | int() | float():
                    name = self.NumberName(unit, name)
                case dict() if unit.get("name"):
                    name = self.NumberName(unit, name["name"])
                case list():
                    unit = random.choice(unit)
                    return self.RUnit(unit, name, zero, **kwargs)
                case _ if is_ast(unit):
                    name = self.NumberName(unit)
                case _:
                    raise Exception(f'This "{unit}" is not vaild!')

        match name:
            case "Z+" | "Int":
                return self.Integer_Positive(unit, zero, **kwargs)
            case "D+":
                return self.Decimal_Positive(unit, zero, **kwargs)
            case "Q+":
                return self.Rational_Positive(unit, zero, **kwargs)
            case "T+":
                return self.Irrational_Positive(unit, zero, **kwargs)
            case "P+":
                return self.Power_Positive(unit, zero, **kwargs)
            case "R+" | "A+" | "C+":
                return self.Real_Positive(unit, zero, **kwargs)
            case "Imag+":
                return self.Imaginary_Positive(unit, zero, **kwargs)
            case "Equation+":
                return self.Equation_Positive(unit, zero, **kwargs)
            case "Set+" | "Coordinate+" | "List+":
                return self.Set_Positive(unit, zero, **kwargs)
            case "S+":
                return self.Symbol_Positive(unit, zero, **kwargs)
            case "Function+":
                return self.Function_Positive(unit, zero, **kwargs)
            case "Attr+":
                unit = (
                    unit
                    if isinstance(unit, dict) and unit
                    else self.ast_expr_dict(unit)
                )

                if unit.get("name") in ("Attr+", "Attr-"):
                    node = unit.get("unit")
                    match node.value:
                        case ast.Call():
                            node.value = self.RUnit(
                                node.value, zero=zero, **kwargs)
                    return node
                return ast.Constant(value=None)
            case (
                "Z-"
                | "D-"
                | "QD-"
                | "T-"
                | "P-"
                | "S-"
                | "Function-"
                | "R-"
                | "A-"
                | "C-"
            ):
                unit = self.RUnit(unit, name.replace("-", "+"), zero, **kwargs)
                return ast.UnaryOp(op=ast.USub(), operand=unit)
            case "Fix+":
                unit = (
                    unit
                    if isinstance(unit, dict) and unit
                    else self.ast_expr_dict(unit)
                )
                return unit.get("unit")
            case "Fix-":
                unit = self.RUnit(unit, "Fix+", zero, **kwargs)
                unit = self.RUnit(unit, "Fix+", zero, **kwargs)
                return ast.UnaryOp(op=ast.USub(), operand=unit)
            case "Q-":
                unit = (
                    unit
                    if isinstance(unit, dict) and unit
                    else self.ast_expr_dict(unit)
                )
                if kwargs.get("rand_unit") != False:
                    unit_rational = self.RUnit(unit, "Q+", zero, **kwargs)
                else:
                    unit_rational = unit.get("unit")
                if any(unit.get(i) == "-" for i in ["neg_p", "neg_q"]):
                    if unit.get("neg_p") == "-":
                        unit_rational.args[0] = ast.UnaryOp(
                            op=ast.USub(), operand=unit_rational.args[0]
                        )
                    if unit.get("neg_q") == "-":
                        unit_rational.args[1] = ast.UnaryOp(
                            op=ast.USub(), operand=unit_rational.args[1]
                        )
                else:
                    match random.randrange(0, 3):
                        case 0 | 1 as index_neg:
                            unit.update(
                                {"neg_p" if index_neg == 0 else "neg_q": "-"})
                        case 2:
                            unit.update({"neg_p": "-", "neg_q": "-"})
                    return self.RUnit(unit, name, zero, rand_unit=False)
                return unit_rational
            case _:
                return ast.Constant(value=None)

    def __int__(self):
        return Eval(self).__int__()

    def __float__(self):
        return Eval(self).__float__()

    def __complex__(self):
        return Eval(self).__complex__()

    def __round__(self):
        return Eval(self).__round__()

    def __trunc__(self):
        return Eval(self).__trunc__()

    def __floor__(self):
        return Eval(self).__floor__()

    def __ceil__(self):
        return Eval(self).__ceil__()

    def __neg__(self):
        return self.Operation(False, Eval(self).__neg__())

    def __pos__(self):
        return Eval(self).__pos__()

    def __abs__(self):
        return Eval(self).__abs__()

    def __invert__(self):
        return Eval(self).__invert__()

    def Operation(self, op, y, **kwargs):
        if (
            (x_operand := Eval(self, eval_BinOp=True))
            and (y_operand := Eval(y, eval_BinOp=True))
            and op
        ):

            x, y = repr(x_operand), repr(y_operand)

            if precedence_arithmetic(op) > precedence_arithmetic(
                x
            ) and precedence_arithmetic(x):
                x = f"({x})"
            elif precedence_arithmetic(
                op, floordiv=True, mod=True
            ) == precedence_arithmetic(x, floordiv=True) == 5 and kwargs.get(
                "reflected"
            ):
                x = f"({x})"

            if precedence_arithmetic(op) > precedence_arithmetic(
                y
            ) and precedence_arithmetic(y):
                y = f"({y})"
            elif precedence_arithmetic(op, floordiv=True) == precedence_arithmetic(
                y, floordiv=True
            ) == 5 and not kwargs.get("reflected"):
                y = f"({y})"

            Expr_eval = None
            if kwargs.get("reflected"):
                Expr_eval = f"{y} {op} {x}"
            else:
                Expr_eval = f"{x} {op} {y}"
            kwargs = self.__dict__.copy()
            kwargs.update({"number": Expr_eval})
            return self.__class__(**kwargs)
        elif Eval(y, eval_BinOp=True) and op == False:

            kwargs = self.__dict__.copy()
            kwargs.update({"number": y})
            return self.__class__(**kwargs)

        if not Eval(self):
            raise ValueError(f"x : '{self}' is not support")

        raise ValueError(f"y : '{y}' is not support")

    def __add__(self, y):
        return self.Operation("+", y)

    def __iadd__(self, y):
        return self.__add__(y)

    def __radd__(self, x):
        return self.Operation("+", x, reflected=True)

    def __sub__(self, y):
        return self.Operation("-", y)

    def __isub__(self, y):
        return self.__sub__(y)

    def __rsub__(self, x):
        return self.Operation("-", x, reflected=True)

    def __mul__(self, y):
        return self.Operation("*", y)

    def __imul__(self, y):
        return self.__mul__(y)

    def __rmul__(self, x):
        return self.Operation("*", x, reflected=True)

    def __truediv__(self, y):
        return self.Operation("/", y)

    def __itruediv__(self, y):
        return self.__truediv__(y)

    def __rtruediv__(self, x):
        return self.Operation("/", x, reflected=True)

    def __floordiv__(self, y):
        return self.Operation("//", y)

    def __ifloordiv__(self, y):
        return self.__floordiv__(y)

    def __rfloordiv__(self, x):
        return self.Operation("//", x, reflected=True)

    def __mod__(self, y):
        return self.Operation("%", y)

    def __imod__(self, y):
        return self.__mod__(y)

    def __rmod__(self, x):
        return self.Operation("%", x, reflected=True)

    def __pow__(self, y):
        return self.Operation("**", y)

    def __ipow__(self, y):
        return self.__pow__(y)

    def __rpow__(self, x):
        return self.Operation("**", x, reflected=True)

    def __matmul__(self, y):
        if (rem := (Eval(self)) % (Eval(y))) == 0 and abs(Eval(self)) != abs(Eval(y)):
            return self

        x_name = self.NumberName(self)
        y_name = self.NumberName(y)
        x = Eval(self)
        y = Eval(y)
        if all(i in ("Z-", "Z+", "D+", "D-") for i in [x_name, y_name]):
            x = abs(x)
            y = abs(y)

            if (
                x > y
                and (x - y) != y
                and x != y
                and all(i in ("Z-", "Z+") for i in [x_name, y_name])
            ):
                x = x - rem
            elif any(i in ("D+", "D-") for i in [x_name, y_name]):
                x = str(decimal.Decimal(str(x)) * decimal.Decimal(str(y)))
            else:
                x = x * y

            if any(i in ("Z-", "D-") for i in [x_name, y_name]):
                x = -x

        return self.Operation(False, x)

    def compare(self, op, y):
        _x = None
        if _x := precedence_arithmetic(str(self), BinOp=False):
            _x = r"{}".format(_x)
        elif _x := self.NumberName(None, self.name, index=True):
            _x = r"{}".format(_x)
        elif _x := Eval(self, eval_BinOp=True):
            _x = r"{}".format(_x)

        _y = None
        if _y := precedence_arithmetic(y, BinOp=False):
            _y = r"{}".format(_y)
        elif _y := self.NumberName(None, y, index=True):
            _y = self.NumberName(None, _y, index=True)
            _y = r"{}".format(_y)
        elif _y := Eval(y, eval_BinOp=True):
            _y = r"{}".format(_y)

        return Eval(f"{_x} {op} {_y}").eval()

    def __lt__(self, x):
        return self.compare("<", x)

    def __le__(self, x):
        return self.compare("<=", x)

    def __gt__(self, x):
        return self.compare(">", x)

    def __ge__(self, x):
        return self.compare(">=", x)

    def __eq__(self, x):
        if (_x := precedence_arithmetic(x, BinOp=False)) and self.name == False:
            y = precedence_arithmetic(str(self), BinOp=False)
            return _x == y
        elif self.NumberName(None, x) and isinstance(self.name, str):
            if any(i in x for i in ["-", "+"]):
                return x == self.name
            else:
                y = f"{self.name}".replace("-", "").replace("+", "")
                return x == y
        elif (_x := Eval(x, eval_BinOp=True)) and (_y := Eval(self, eval_BinOp=True)):
            return _x == _y
        return False

    def __ne__(self, x):
        return self.compare("!=", x)

    def __hash__(self):
        return hash(str(self))


if "__main__" == __name__:
    pass
