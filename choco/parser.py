import time
from typing import List, Union

from xdsl.dialects.builtin import ModuleOp
from xdsl.ir import Operation

import choco.dialects.choco_ast as ast
from choco.lexer import Lexer, Token, TokenKind


class Parser:
    """
    A Simple ChocoPy Parser

    Parse the given tokens from the lexer and call the xDSL API to create an AST.
    """

    def __init__(self, lexer: Lexer):
        """
        Create a new parser.

        Initialize parser with the corresponding lexer.
        """
        self.lexer = lexer

    def check(self, expected: Union[List[TokenKind], TokenKind]) -> bool:
        """
                Check that the next token is of a given kind. If a list of n TokenKinds
                is given, check that the next n TokenKinds match the next expected
                ones.

                :param expected: The kind of the token we expect or a list of expected
                                 token kinds if we look ahead more than one token at
                                 a time.
                :returns: True if the next token has the expected token kind, False
        ￼                 otherwise.
        """

        if isinstance(expected, list):
            tokens = self.lexer.peek(len(expected))
            assert isinstance(tokens, list), "List of tokens expected"
            return all([tok.kind == type_ for tok, type_ in zip(tokens, expected)])

        token = self.lexer.peek()
        assert isinstance(token, Token), "Single token expected"
        return token.kind == expected

    def match(self, expected: TokenKind) -> Token:
        """
        Match a token by first checking the token kind. In case the token is of
        the expected kind, we consume the token.  If a token with an unexpected
        token kind is encountered, an error is reported by raising an
        exception.

        The exception shows information about the line where the token was expected.

        :param expected: The kind of the token we expect.
        :returns: The consumed token if the next token has the expected token
                  kind, otherwise a parsing error is reported.
        """

        if self.check(expected):
            token = self.lexer.peek()
            assert isinstance(token, Token), "A single token expected"
            self.lexer.consume()
            print(token.kind)
            return token

        token = self.lexer.peek()
        print("error", token.kind)
        assert isinstance(token, Token), "A single token expected"
        print(f"Error: token of kind {expected} not found.")
        exit(0)

    def parse_program(self) -> ModuleOp:
        """
        Parse a ChocoPy program.

        program ::= def_seq stmt_seq EOF

        TODO: Not fully implemented.
        :returns: The AST of a ChocoPy Program.
        """

        defs = self.parse_multi_opt_var_or_func_def()

        stmts = self.parse_multi_stmt()

        self.match(TokenKind.EOF)

        return ModuleOp([ast.Program(defs, stmts)])

    def parse_multi_opt_var_or_func_def(self) -> List[Operation]:
        """
        Parse a sequence of function and variable definitions.

        def_seq ::= [func_def | var_def]*

        TODO: Not fully implemented.
        :returns: A list of function and variable definitions.
        """

        # defs: List[Operation] = []
        #
        # while self.check(TokenKind.DEF):
        #     func_def = self.parse_function()
        #     defs.append(func_def)
        #
        # return defs
        self.check_assign_error()

        self.check_unexpected_indent_error()
        if self.check([TokenKind.IDENTIFIER, TokenKind.COLON]) or self.check(TokenKind.DEF):
            return self.parse_multi_opt_var_or_func_def_helper()
        self.check_assign_error()
        return []

    def parse_multi_opt_var_or_func_def_helper(self) -> List[Operation]:
        self.check_unexpected_indent_error()
        operation = self.parse_var_or_func()
        self.check_unexpected_indent_error()
        if self.check([TokenKind.IDENTIFIER, TokenKind.COLON]) or self.check(TokenKind.DEF):
            lists = self.parse_multi_opt_var_or_func_def_helper()
            lists.insert(0, operation)
            return lists
        else:
            return [operation]

    def parse_var_or_func(self) -> Operation:
        if self.check(TokenKind.IDENTIFIER):
            return self.parse_var()
        elif self.check(TokenKind.DEF):
            return self.parse_function()

    def parse_multi_stmt(self) -> List[Operation]:
        self.check_assign_error()
        self.check_unexpected_indent_error()
        if self.is_stmt_first_set():
            return self.parse_multi_stmt_helper()
        self.check_assign_error()
        return []

    def parse_multi_stmt_helper(self) -> List[Operation]:
        self.check_unexpected_indent_error()
        stmt = self.parse_stmt()
        self.check_unexpected_indent_error()
        if self.is_stmt_first_set():
            lists = self.parse_multi_stmt_helper()
            lists.insert(0, stmt)
            return lists
        return [stmt]

    def parse_function(self) -> Operation:
        """
        Parse a function definition.

                   func_def := `def` ID `(` `)` `:` NEWLINE INDENT func_body DEDENT
                  func_body := stmt_seq

        The above definition is incomplete.

        TODO: Not fully implemented.
        :return: Operation
        """
        self.match(TokenKind.DEF)

        function_name = self.match(TokenKind.IDENTIFIER)

        self.check_lround()
        self.match(TokenKind.LROUNDBRACKET)

        # Function parameters
        parameters: List[Operation] = []
        if self.check(TokenKind.IDENTIFIER):
            parameters = self.parse_argument()

        self.check_rround()
        self.match(TokenKind.RROUNDBRACKET)
        return_type: Operation
        # Return type: default is <None>.
        if self.check(TokenKind.RARROW):
            return_type = self.parse_return_type_opt()
        else:
            return_type = ast.TypeName('<None>')
        self.check_colon_error()
        self.match(TokenKind.COLON)

        self.check_newline_error()
        self.match(TokenKind.NEWLINE)
        self.check_func_indent_error()
        self.match(TokenKind.INDENT)

        func_body = []
        if self.check(TokenKind.GLOBAL) or self.check(TokenKind.NONLOCAL) or self.check(
                TokenKind.IDENTIFIER) or self.is_stmt_first_set():
            func_body: List[Operation] = self.parse_func_body()

        self.check_assign_error()

        # stmt_seq = self.parse_stmt_seq()
        # if not stmt_seq:
        #     raise Exception(
        #         'Error: Function body should have at least one statement.')

        # func_body = defs_and_decls + stmt_seq
        self.match(TokenKind.DEDENT)

        return ast.FuncDef(function_name.value, parameters, return_type, func_body)

    def is_expr_first_set(self) -> bool:
        """
        Check if the next token is in the first set of an expression.

        TODO: Not fully implemented.
        """
        return self.check(TokenKind.IDENTIFIER) or self.check(TokenKind.LSQUAREBRACKET) or self.check(
            TokenKind.LROUNDBRACKET) or self.check(TokenKind.MINUS) or self.check(TokenKind.NONE) or self.check(
            TokenKind.TRUE) or self.check(TokenKind.FALSE) or self.check(TokenKind.INTEGER) or self.check(
            TokenKind.STRING) or self.check(TokenKind.NOT)

    def is_stmt_first_set(self) -> bool:
        """
        Check if the next token is in the first set of a statement.

        TODO: Not fully implemented.
        """
        return (self.is_expr_first_set() or self.check(TokenKind.PASS) or self.check(TokenKind.IF) or
                self.check(TokenKind.WHILE) or self.check(TokenKind.FOR) or self.check(TokenKind.RETURN))

    def parse_stmt(self) -> Operation:
        """Parse a statement.

        stmt := simple_stmt NEWLINE

        The above definition is incomplete.

        TODO: Not fully implemented.
        :return: Statement as operation
        """
        if self.check(TokenKind.PASS) or self.is_expr_first_set() or self.check(TokenKind.RETURN):

            simple_stmt = self.parse_simple_stmt()
            if self.check(TokenKind.EQ) or self.check(TokenKind.NE) or self.check(TokenKind.LE) or \
                    self.check(TokenKind.GE) or self.check(TokenKind.LT) or self.check(TokenKind.GT) or self.check(
                TokenKind.IS):
                self.lexer.self_finish()
                content = self.lexer.tokenizer.content
                info = content[len(content) - 1]
                row = info.line
                column = info.current_token
                mystr = info.str_at_line
                print("SyntaxError (line", str(row) + ", column",
                      str(column) + "): Comparison operators are not associative.")
                print(">>>" + mystr)
                print(">>>", end="")
                for i in range(0, column - 1):
                    print("-", end="")
                print("^")
                exit(0)
            self.check_unmatched_parenthesis()
            self.check_variable_declaration_error()
            self.check_newline_error()
            self.match(TokenKind.NEWLINE)
            return simple_stmt
        elif self.check(TokenKind.IF):
            self.match(TokenKind.IF)
            self.check_expr_error()
            condition = self.parse_expr()
            self.check_colon_error()
            self.match(TokenKind.COLON)
            then = self.parse_block()
            orelse = []
            if self.check(TokenKind.ELIF) or self.check(TokenKind.ELSE):
                orelse = self.parse_multi_elif_and_else_opt()
            return ast.If(condition, then, orelse)
        elif self.check(TokenKind.WHILE):
            self.match(TokenKind.WHILE)
            self.check_expr_error()
            condition = self.parse_expr()
            self.check_colon_error()
            self.match(TokenKind.COLON)
            body = self.parse_block()
            return ast.While(condition, body)
        elif self.check(TokenKind.FOR):
            self.match(TokenKind.FOR)
            iter_name = self.match(TokenKind.IDENTIFIER)
            self.check_in_error()
            self.match(TokenKind.IN)
            self.check_expr_error()
            iter_range = self.parse_expr()
            self.check_colon_error()
            self.match(TokenKind.COLON)
            body = self.parse_block()
            value = ast.For(iter_name.value, iter_range, body)
            return value

    def parse_simple_stmt(self) -> Operation:
        """Parse a simple statement.

        stmt := `pass`

        The above definition is incomplete.

        TODO: Not fully implemented.
        :return: Statement as operation
        """
        if self.is_expr_first_set():
            # if self.check(TokenKind.IDENTIFIER):
            #     return ast.ExprName(self.match(TokenKind.IDENTIFIER).value)
            # return self.parse_literal()
            self.check_expr_error()
            expr = self.parse_expr()
            if self.check(TokenKind.ASSIGN):
                self.match(TokenKind.ASSIGN)
                operation = self.parse_expr_eq_pos_helper()
                return ast.Assign(expr, operation)

            return expr
        elif self.check(TokenKind.RETURN):
            self.match(TokenKind.RETURN)
            if self.is_expr_first_set():
                self.check_expr_error()
                operation = self.parse_expr()
                return ast.Return(operation)
            return ast.Return(None)
        elif self.check(TokenKind.PASS):
            self.match(TokenKind.PASS)
            return ast.Pass()

    def parse_literal(self) -> ast.Literal:

        if self.check(TokenKind.NONE):
            self.match(TokenKind.NONE)
            return ast.Literal(None)
        if self.check(TokenKind.TRUE):
            self.match(TokenKind.TRUE)
            return ast.Literal(True)
        if self.check(TokenKind.FALSE):
            self.match(TokenKind.FALSE)
            return ast.Literal(False)
        if self.check(TokenKind.STRING):
            token: Token = self.match(TokenKind.STRING)
            return ast.Literal(token.value)

        if self.check(TokenKind.INTEGER):
            token: Token = self.match(TokenKind.INTEGER)
            return ast.Literal(token.value)

    def parse_func_body(self) -> List[Operation]:
        assignment = self.parse_all_variables_assignment()
        stmt = []
        if self.is_stmt_first_set():
            stmt = self.parse_stmt_pos()
        if not stmt:
            self.check_no_statement_error()
        return assignment + stmt

    def parse_all_variables_assignment(self) -> List[Operation]:
        if self.check(TokenKind.GLOBAL) or self.check(TokenKind.NONLOCAL) or self.check(
                [TokenKind.IDENTIFIER, TokenKind.COLON]):
            return self.parse_all_variables_assignment_helper()
        return []

    def parse_all_variables_assignment_helper(self) -> List[Operation]:
        operation = self.parse_global_or_nonlocal_or_var()
        self.check_unexpected_indent_error()
        if self.check(TokenKind.GLOBAL) or self.check(TokenKind.NONLOCAL) or self.check(
                [TokenKind.IDENTIFIER, TokenKind.COLON]):
            list_operation = self.parse_all_variables_assignment_helper()
            list_operation.insert(0, operation)
            return list_operation
        else:
            lists: List[Operation] = [operation]
            return lists

    def parse_stmt_pos(self) -> List[Operation]:
        self.check_unexpected_indent_error()
        operation = self.parse_stmt()
        self.check_unexpected_indent_error()
        if self.is_stmt_first_set():
            lists = self.parse_stmt_pos()
            lists.insert(0, operation)
            return lists
        else:
            return [operation]

    def parse_global_or_nonlocal_or_var(self) -> Operation:
        if self.check(TokenKind.GLOBAL):
            global_decl = self.parse_global_decl()
            return global_decl
        if self.check(TokenKind.NONLOCAL):
            non_local = self.parse_nonlocal_decl()
            return non_local
        if self.check(TokenKind.IDENTIFIER):
            var_def = self.parse_var()
            return var_def

    def parse_global_decl(self) -> Operation:
        self.match(TokenKind.GLOBAL)
        self.check_global_or_nonlocal_error()
        id = self.match(TokenKind.IDENTIFIER)
        self.check_newline_error()
        self.match(TokenKind.NEWLINE)
        return ast.GlobalDecl(id.value)

    def parse_nonlocal_decl(self) -> Operation:
        self.match(TokenKind.NONLOCAL)
        self.check_global_or_nonlocal_error()
        id = self.match(TokenKind.IDENTIFIER)
        self.check_newline_error()
        self.match(TokenKind.NEWLINE)

        return ast.NonLocalDecl(id.value)

    def parse_var(self) -> Operation:
        typed_var = self.parse_typed_var()
        if self.check(TokenKind.ASSIGN):
            self.match(TokenKind.ASSIGN)
        elif not self.check(TokenKind.ASSIGN):
            self.lexer.self_finish()
            content = self.lexer.tokenizer.content
            info = content[len(content) - 1]
            row = info.line
            column = info.current_token
            mystr = info.str_at_line
            print("SyntaxError (line", str(row) + ", column",
                  str(column) + "): token of kind TokenKind.ASSIGN not found.")
            print(">>>" + mystr)
            print(">>>", end="")
            for i in range(0, column - 1):
                print("-", end="")
            print("^")
            exit(0)
        literal = self.parse_literal()
        self.check_newline_error()
        self.match(TokenKind.NEWLINE)
        return ast.VarDef(typed_var, literal)

    def parse_typed_var(self) -> ast.TypedVar:
        id = self.match(TokenKind.IDENTIFIER)
        self.check_colon_error()
        self.match(TokenKind.COLON)
        type_name = self.parse_type()

        return ast.TypedVar(id.value, type_name)

    def parse_type(self) -> Operation:
        type_token: Token
        if self.check(TokenKind.OBJECT):
            type_token = self.match(TokenKind.OBJECT)
        elif self.check(TokenKind.INT):
            type_token = self.match(TokenKind.INT)
        elif self.check(TokenKind.BOOL):
            type_token = self.match(TokenKind.BOOL)
        elif self.check(TokenKind.STR):
            type_token = self.match(TokenKind.STR)
        elif self.check(TokenKind.LSQUAREBRACKET):
            self.match(TokenKind.LSQUAREBRACKET)
            type_new = self.parse_type()
            self.check_rsqaure_error()
            self.match(TokenKind.RSQUAREBRACKET)

            return ast.ListType(type_new)
        else:
            self.lexer.self_finish()
            content = self.lexer.tokenizer.content
            info = content[len(content) - 1]
            row = info.line
            column = info.current_token
            mystr = info.str_at_line
            print("SyntaxError (line", str(row) + ", column", str(column) + "): Unknown type.")
            print(">>>" + mystr)
            print(">>>", end="")
            for i in range(0, column - 1):
                print("-", end="")
            print("^")
            exit(0)

        return ast.TypeName(type_token.value)

    def parse_argument(self) -> List[Operation]:
        typed_var = self.parse_typed_var()
        lists = []
        if self.check(TokenKind.COMMA):
            lists = self.parse_multi_typed_var()
        elif self.is_expr_first_set():
            self.lexer.self_finish()
            content = self.lexer.tokenizer.content
            info = content[len(content) - 1]
            row = info.line
            column = info.current_token
            mystr = info.str_at_line
            print("SyntaxError (line", str(row) + ", column", str(column) + "): expression found, but comma expected.")
            print(">>>" + mystr)
            print(">>>", end="")
            for i in range(0, column - 1):
                print("-", end="")
            print("^")
            exit(0)
        lists.insert(0, typed_var)
        return lists

    def parse_multi_typed_var(self) -> List[Operation]:
        return self.parse_multi_typed_var_helper()

    def parse_multi_typed_var_helper(self) -> List[Operation]:
        self.match(TokenKind.COMMA)
        typed_var = self.parse_typed_var()
        if self.check(TokenKind.COMMA):
            lists = self.parse_multi_typed_var_helper()
            lists.insert(0, typed_var)
            return lists
        elif self.is_expr_first_set():
            self.lexer.self_finish()
            content = self.lexer.tokenizer.content
            info = content[len(content) - 1]
            row = info.line
            column = info.current_token
            mystr = info.str_at_line
            print("SyntaxError (line", str(row) + ", column", str(column) + "): expression found, but comma expected.")
            print(">>>" + mystr)
            print(">>>", end="")
            for i in range(0, column - 1):
                print("-", end="")
            print("^")
            exit(0)
        else:
            lists = [typed_var]
            return lists

    def parse_return_type_opt(self) -> Operation:
        self.match(TokenKind.RARROW)
        return_type = self.parse_type()
        return return_type

    def parse_index_expr(self) -> Operation:
        value = self.parse_cexpr()
        self.check_lsqaure_error()
        self.match(TokenKind.LSQUAREBRACKET)
        self.check_expr_error()
        index = self.parse_expr()
        self.check_rsqaure_error()
        self.match(TokenKind.RSQUAREBRACKET)
        return ast.IndexExpr(value, index)

    def parse_expr(self) -> Operation:
        self.check_expr_error()
        then = self.parse_expr_first()

        if self.check(TokenKind.IF):
            self.match(TokenKind.IF)
            self.check_expr_error()
            condition = self.parse_expr()
            self.check_else_error()
            self.match(TokenKind.ELSE)
            self.check_expr_error()
            orelse = self.parse_expr()
            return ast.IfExpr(condition, then, orelse)

        return then

    def parse_expr_first(self) -> Operation:
        self.check_expr_error()
        lhs = self.parse_expr_second()
        if self.check(TokenKind.OR):
            value = self.parse_expr_first_helper(lhs)
            return value

        return lhs

    def parse_expr_first_helper(self, lhs: Operation) -> Operation:
        if self.check(TokenKind.OR):
            or_token = self.match(TokenKind.OR)
            self.check_expr_error()
            rhs = self.parse_expr_second()
            new_lhs = ast.BinaryExpr(or_token.value, lhs, rhs)
            if self.check(TokenKind.OR):
                return self.parse_expr_first_helper(new_lhs)
            return new_lhs

    def parse_expr_second(self) -> Operation:
        self.check_expr_error()
        lhs = self.parse_expr_third()

        if self.check(TokenKind.AND):
            value = self.parse_expr_second_helper(lhs)
            return value
        return lhs

    def parse_expr_second_helper(self, lhs: Operation) -> Operation:
        if self.check(TokenKind.AND):
            and_token = self.match(TokenKind.AND)
            self.check_expr_error()
            rhs = self.parse_expr_third()
            new_lhs = ast.BinaryExpr(and_token.value, lhs, rhs)
            if self.check(TokenKind.AND):
                return self.parse_expr_second_helper(new_lhs)
            return new_lhs

    def parse_expr_third(self) -> Operation:
        if self.check(TokenKind.NOT):
            not_token = self.match(TokenKind.NOT)
            self.check_expr_error()
            operation = self.parse_expr_third()
            return ast.UnaryExpr(not_token.value, operation)
        return self.parse_cexpr()

    """
        因为比较运算符是不能像 x==y<z这样的，因此直接两边变成cexpr_first，但没考虑x==y + y==z这种情况
    """

    def parse_cexpr(self) -> Operation:
        self.check_expr_error()
        operation = self.parse_cexpr_first()
        if self.check(TokenKind.EQ):
            eq = self.match(TokenKind.EQ)
            rhs = self.parse_cexpr_first()
            return ast.BinaryExpr(eq.value, operation, rhs)
        elif self.check(TokenKind.NE):
            ne = self.match(TokenKind.NE)
            rhs = self.parse_cexpr_first()
            return ast.BinaryExpr(ne.value, operation, rhs)
        elif self.check(TokenKind.LE):
            le = self.match(TokenKind.LE)
            rhs = self.parse_cexpr_first()
            return ast.BinaryExpr(le.value, operation, rhs)
        elif self.check(TokenKind.GE):
            ge = self.match(TokenKind.GE)
            rhs = self.parse_cexpr_first()
            return ast.BinaryExpr(ge.value, operation, rhs)
        elif self.check(TokenKind.LT):
            lt = self.match(TokenKind.LT)
            rhs = self.parse_cexpr_first()
            return ast.BinaryExpr(lt.value, operation, rhs)
        elif self.check(TokenKind.GT):
            gt = self.match(TokenKind.GT)
            rhs = self.parse_cexpr_first()
            return ast.BinaryExpr(gt.value, operation, rhs)
        elif self.check(TokenKind.IS):
            is_token = self.match(TokenKind.IS)
            rhs = self.parse_cexpr_first()
            return ast.BinaryExpr(is_token.value, operation, rhs)
        return operation

    # def parse_cexpr(self) -> Operation:
    #     lhs = self.parse_cexpr_first()
    #     if self.check(TokenKind.EQ) or self.check(TokenKind.NE) or self.check(TokenKind.LE) or \
    #         self.check(TokenKind.GE) or self.check(TokenKind.LT) or self.check(TokenKind.GT) or self.check(TokenKind.IS):
    #         value = self.parse_cexpr_helper(lhs)
    #         return value
    #     return lhs
    #
    # def parse_cexpr_helper(self, lhs: Operation):
    #     if self.check(TokenKind.EQ):
    #         eq = self.match(TokenKind.EQ)
    #         rhs = self.parse_cexpr_first()
    #         return ast.BinaryExpr(eq.value, lhs, rhs)
    #     elif self.check(TokenKind.NE):
    #         ne = self.match(TokenKind.NE)
    #         rhs = self.parse_cexpr_first()
    #         return ast.BinaryExpr(ne.value, lhs, rhs)
    #     elif self.check(TokenKind.LE):
    #         le = self.match(TokenKind.LE)
    #         rhs = self.parse_cexpr_first()
    #         return ast.BinaryExpr(le.value, lhs, rhs)
    #     elif self.check(TokenKind.GE):
    #         ge = self.match(TokenKind.GE)
    #         rhs = self.parse_cexpr_first()
    #         return ast.BinaryExpr(ge.value, lhs, rhs)
    #     elif self.check(TokenKind.LT):
    #         lt = self.match(TokenKind.LT)
    #         rhs = self.parse_cexpr_first()
    #         return ast.BinaryExpr(lt.value, lhs, rhs)
    #     elif self.check(TokenKind.GT):
    #         gt = self.match(TokenKind.GT)
    #         rhs = self.parse_cexpr_first()
    #         return ast.BinaryExpr(gt.value, lhs, rhs)
    #     elif self.check(TokenKind.IS):
    #         is_token = self.match(TokenKind.IS)
    #         rhs = self.parse_cexpr_first()
    #         return ast.BinaryExpr(is_token.value, lhs, rhs)
    #     return lhs

    # def parse_cexpr_first(self) -> Operation:
    #     operation = self.parse_cexpr_second()
    #     if self.check(TokenKind.PLUS):
    #         plus = self.match(TokenKind.PLUS)
    #         rhs = self.parse_cexpr_first()
    #         return ast.BinaryExpr(plus.value, operation, rhs)
    #     elif self.check(TokenKind.MINUS):
    #         minus = self.match(TokenKind.MINUS)
    #         rhs = self.parse_cexpr_first()
    #         return ast.BinaryExpr(minus.value, operation, rhs)
    #     return operation
    def parse_cexpr_first(self) -> Operation:
        self.check_expr_error()
        lhs = self.parse_cexpr_second()
        if self.check(TokenKind.PLUS) or self.check(TokenKind.MINUS):
            value = self.parse_cexpr_first_helper(lhs)
            return value
        return lhs

    def parse_cexpr_first_helper(self, lhs: Operation) -> Operation:
        if self.check(TokenKind.PLUS):
            plus = self.match(TokenKind.PLUS)
            self.check_expr_error()
            rhs = self.parse_cexpr_second()
            new_lhs = ast.BinaryExpr(plus.value, lhs, rhs)
            if self.check(TokenKind.PLUS) or self.check(TokenKind.MINUS):
                return self.parse_cexpr_first_helper(new_lhs)
            return new_lhs
        elif self.check(TokenKind.MINUS):
            plus = self.match(TokenKind.MINUS)
            self.check_expr_error()
            rhs = self.parse_cexpr_second()
            new_lhs = ast.BinaryExpr(plus.value, lhs, rhs)
            if self.check(TokenKind.PLUS) or self.check(TokenKind.MINUS):
                return self.parse_cexpr_first_helper(new_lhs)
            return new_lhs

    # def parse_cexpr_second(self) -> Operation:
    #     operation = self.parse_cexpr_third()
    #     if self.check(TokenKind.MUL):
    #         mul = self.match(TokenKind.MUL)
    #         rhs = self.parse_cexpr_second()
    #         return ast.BinaryExpr(mul.value, operation, rhs)
    #     elif self.check(TokenKind.DIV):
    #         div = self.match(TokenKind.DIV)
    #         rhs = self.parse_cexpr_second()
    #         return ast.BinaryExpr(div.value, operation, rhs)
    #     elif self.check(TokenKind.MOD):
    #         mod = self.match(TokenKind.MOD)
    #         rhs = self.parse_cexpr_second()
    #         return ast.BinaryExpr(mod.value, operation, rhs)
    #     return operation
    def parse_cexpr_second(self) -> Operation:
        self.check_expr_error()
        lhs = self.parse_cexpr_third()
        if self.check(TokenKind.MUL) or self.check(TokenKind.DIV) or self.check(TokenKind.MOD):
            return self.parse_cexpr_second_helper(lhs)
        return lhs

    def parse_cexpr_second_helper(self, lhs: Operation) -> Operation:
        if self.check(TokenKind.MUL):
            mul = self.match(TokenKind.MUL)
            self.check_expr_error()
            rhs = self.parse_cexpr_third()
            new_lhs = ast.BinaryExpr(mul.value, lhs, rhs)
            if self.check(TokenKind.MUL) or self.check(TokenKind.DIV) or self.check(TokenKind.MOD):
                return self.parse_cexpr_second_helper(new_lhs)
            return new_lhs
        elif self.check(TokenKind.DIV):
            div = self.match(TokenKind.DIV)
            self.check_expr_error()
            rhs = self.parse_cexpr_third()
            new_lhs = ast.BinaryExpr(div.value, lhs, rhs)
            if self.check(TokenKind.MUL) or self.check(TokenKind.DIV) or self.check(TokenKind.MOD):
                return self.parse_cexpr_second_helper(new_lhs)
            return new_lhs
        elif self.check(TokenKind.MOD):
            mod = self.match(TokenKind.MOD)
            self.check_expr_error()
            rhs = self.parse_cexpr_third()
            new_lhs = ast.BinaryExpr(mod.value, lhs, rhs)
            if self.check(TokenKind.MUL) or self.check(TokenKind.DIV) or self.check(TokenKind.MOD):
                return self.parse_cexpr_second_helper(new_lhs)
            return new_lhs

    def parse_cexpr_third(self) -> Operation:
        value: Operation = None

        self.check_unexpected_indent_error()
        if self.check([TokenKind.IDENTIFIER, TokenKind.LROUNDBRACKET]):
            id = self.match(TokenKind.IDENTIFIER)
            self.match(TokenKind.LROUNDBRACKET)
            lists = self.parse_multi_expr_opt()

            self.check_rround()
            self.match(TokenKind.RROUNDBRACKET)
            value = ast.CallExpr(id.value, lists)

        elif self.check(TokenKind.IDENTIFIER):
            value = ast.ExprName(self.match(TokenKind.IDENTIFIER).value)
        elif self.check(TokenKind.NONE) or self.check(TokenKind.TRUE) or \
                self.check(TokenKind.FALSE) or self.check(TokenKind.INTEGER) \
                or self.check(TokenKind.STRING):
            value = self.parse_literal()
        elif self.check(TokenKind.LSQUAREBRACKET):
            self.check_lsqaure_error()
            self.match(TokenKind.LSQUAREBRACKET)
            operations = []
            if self.is_expr_first_set():
                operations = self.parse_multi_expr_opt()
            self.check_rsqaure_error()
            self.match(TokenKind.RSQUAREBRACKET)
            value = ast.ListExpr(operations)

        elif self.check(TokenKind.LROUNDBRACKET):
            self.match(TokenKind.LROUNDBRACKET)
            value = self.parse_expr()
            self.check_rround()
            self.match(TokenKind.RROUNDBRACKET)
        elif self.check(TokenKind.MINUS):
            minus = self.match(TokenKind.MINUS)
            self.check_expr_error()
            operation = self.parse_cexpr_third()
            value = ast.UnaryExpr(minus.value, operation)

        if self.check(TokenKind.LSQUAREBRACKET):
            lists = self.parse_cexpr_fourth()
            for i in lists:
                value = ast.IndexExpr(value, i)
            return value
        else:
            return value

    def parse_cexpr_fourth(self) -> List[Operation]:
        self.check_lsqaure_error()
        self.match(TokenKind.LSQUAREBRACKET)
        self.check_expr_error()
        operation = self.parse_expr()
        self.check_rsqaure_error()
        self.match(TokenKind.RSQUAREBRACKET)
        if self.check(TokenKind.LSQUAREBRACKET):
            lists = self.parse_cexpr_fourth()
            lists.insert(0, operation)
            return lists
        else:
            return [operation]

    def parse_multi_expr_opt(self) -> List[Operation]:
        self.check_expr_error()
        operation = self.parse_expr()
        if self.check(TokenKind.COMMA):
            lists = self.parse_multi_expr()
            lists.insert(0, operation)
            return lists
        elif self.is_expr_first_set():
            self.lexer.self_finish()
            content = self.lexer.tokenizer.content
            info = content[len(content) - 1]
            row = info.line
            column = info.current_token
            mystr = info.str_at_line
            print("SyntaxError (line", str(row) + ", column", str(column) + "): expression found, but comma expected.")
            print(">>>" + mystr)
            print(">>>", end="")
            for i in range(0, column - 1):
                print("-", end="")
            print("^")
            exit(0)
        else:
            return [operation]

    def parse_multi_expr(self) -> List[Operation]:
        return self.parse_multi_expr_helper()

    def parse_multi_expr_helper(self) -> List[Operation]:
        self.match(TokenKind.COMMA)
        self.check_expr_error()
        operation = self.parse_expr()
        if self.check(TokenKind.COMMA):
            lists = self.parse_multi_expr_helper()
            lists.insert(0, operation)
            return lists
        elif self.is_expr_first_set():
            self.lexer.self_finish()
            content = self.lexer.tokenizer.content
            info = content[len(content) - 1]
            row = info.line
            column = info.current_token
            mystr = info.str_at_line
            print("SyntaxError (line", str(row) + ", column", str(column) + "): expression found, but comma expected.")
            print(">>>" + mystr)
            print(">>>", end="")
            for i in range(0, column - 1):
                print("-", end="")
            print("^")
            exit(0)
        else:
            return [operation]

    def parse_block(self) -> List[Operation]:
        self.check_newline_error()
        self.match(TokenKind.NEWLINE)
        self.check_block_indent_error()
        self.match(TokenKind.INDENT)
        self.check_unexpected_indent_error()
        lists = self.parse_stmt_pos()
        self.check_unexpected_indent_error()
        self.match(TokenKind.DEDENT)

        return lists

    def parse_multi_elif_and_else_opt(self) -> List[Operation]:
        return self.parse_multi_elif_and_else_opt_helper()

    def parse_multi_elif_and_else_opt_helper(self) -> List[Operation]:
        if self.check(TokenKind.ELIF):
            self.match(TokenKind.ELIF)
            self.check_expr_error()
            condition = self.parse_expr()
            self.check_colon_error()
            self.match(TokenKind.COLON)
            then = self.parse_block()
            orelse = self.parse_multi_elif_and_else_opt_helper()
            return [ast.If(condition, then, orelse)]
        elif self.check(TokenKind.ELSE):
            self.match(TokenKind.ELSE)
            self.check_colon_error()
            self.match(TokenKind.COLON)
            orelse = self.parse_block()
            return orelse
        else:
            return []

    def parse_expr_eq_pos_helper(self) -> Operation:
        self.check_expr_error()
        operation = self.parse_expr()
        if self.check(TokenKind.ASSIGN):
            self.match(TokenKind.ASSIGN)
            value = self.parse_expr_eq_pos_helper()
            return ast.Assign(operation, value)
        return operation

    def check_expr_error(self):

        if not self.is_expr_first_set():
            self.lexer.self_finish()
            content = self.lexer.tokenizer.content
            info = content[len(content) - 1]
            row = info.line
            column = info.current_token
            mystr = info.str_at_line
            # content = self.lexer.tokenizer.content
            # for i in content:
            #     print(i.line, i.column, i.current_token - 1, i.str_at_line)
            print("SyntaxError (line", str(row) + ", column", str(column) + "): Expected expression.")
            print(">>>" + mystr)
            print(">>>", end="")
            for i in range(0, column - 1):
                print("-", end="")
            print("^")
            exit(0)

    def check_assign_error(self):
        if self.check(TokenKind.ASSIGN):
            # [row, column, mystr] = self.lexer.return_row_column()
            self.lexer.self_finish()
            content = self.lexer.tokenizer.content
            info = content[len(content) - 1]
            row = info.line
            column = info.current_token
            mystr = info.str_at_line
            print("SyntaxError (line", str(row) + ", column", str(column) + "): No left-hand side in assign statement.")
            print(">>>" + mystr)
            print(">>>", end="")
            for i in range(0, column - 1):
                print("-", end="")
            print("^")
            exit(0)

    def check_colon_error(self):
        if not self.check(TokenKind.COLON):
            # [row, column, mystr] = self.lexer.return_row_column()
            self.lexer.self_finish()
            content = self.lexer.tokenizer.content
            info = content[len(content) - 1]
            row = info.line
            column = info.current_token
            mystr = info.str_at_line
            print("SyntaxError (line", str(row) + ", column",
                  str(column) + "): token of kind TokenKind.COLON not found.")
            print(">>>" + mystr)
            print(">>>", end="")
            for i in range(0, column - 1):
                print("-", end="")
            print("^")
            exit(0)
        # pass

    def check_else_error(self):
        if not self.check(TokenKind.ELSE):
            # [row, column, mystr] = self.lexer.return_row_column()
            self.lexer.self_finish()
            content = self.lexer.tokenizer.content
            info = content[len(content) - 1]
            row = info.line
            column = info.current_token
            mystr = info.str_at_line
            print("SyntaxError (line", str(row) + ", column",
                  str(column) + "): token of kind TokenKind.ELSE not found.")
            print(">>>" + mystr)
            print(">>>", end="")
            for i in range(0, column - 1):
                print("-", end="")
            print("^")
            exit(0)

    def check_global_or_nonlocal_error(self):
        if not self.check(TokenKind.IDENTIFIER):
            # [row, column, mystr] = self.lexer.return_row_column()
            self.lexer.self_finish()
            content = self.lexer.tokenizer.content
            info = content[len(content) - 1]
            row = info.line
            column = info.current_token
            mystr = info.str_at_line
            print("SyntaxError (line", str(row) + ", column",
                  str(column) + "): token of kind TokenKind.IDENTIFIER not found.")
            print(">>>" + mystr)
            print(">>>", end="")
            for i in range(0, column - 1):
                print("-", end="")
            print("^")
            exit(0)

    def check_in_error(self):
        if not self.check(TokenKind.IN):
            # [row, column, mystr] = self.lexer.return_row_column()
            self.lexer.self_finish()
            content = self.lexer.tokenizer.content
            info = content[len(content) - 1]
            row = info.line
            column = info.current_token
            mystr = info.str_at_line
            print("SyntaxError (line", str(row) + ", column", str(column) + "): token of kind TokenKind.IN not found.")
            print(">>>" + mystr)
            print(">>>", end="")
            for i in range(0, column - 1):
                print("-", end="")
            print("^")
            exit(0)

    def check_lround(self):
        if not self.check(TokenKind.LROUNDBRACKET):
            # [row, column, mystr] = self.lexer.return_row_column()
            self.lexer.self_finish()
            content = self.lexer.tokenizer.content
            info = content[len(content) - 1]
            row = info.line
            column = info.current_token
            mystr = info.str_at_line
            print("SyntaxError (line", str(row) + ", column",
                  str(column) + "): token of kind TokenKind.LROUNDBRACKET not found.")
            print(">>>" + mystr)
            print(">>>", end="")
            for i in range(0, column - 1):
                print("-", end="")
            print("^")
            exit(0)

    def check_rround(self):
        if not self.check(TokenKind.RROUNDBRACKET):
            # [row, column, mystr] = self.lexer.return_row_column()
            self.lexer.self_finish()
            content = self.lexer.tokenizer.content
            info = content[len(content) - 1]
            row = info.line
            column = info.current_token
            mystr = info.str_at_line
            print("SyntaxError (line", str(row) + ", column",
                  str(column) + "): token of kind TokenKind.RROUNDBRACKET not found.")
            print(">>>" + mystr)
            print(">>>", end="")
            for i in range(0, column - 1):
                print("-", end="")
            print("^")
            exit(0)

    def check_newline_error(self):
        if not self.check(TokenKind.NEWLINE):
            # [row, column, mystr] = self.lexer.return_row_column()
            self.lexer.self_finish()
            content = self.lexer.tokenizer.content
            info = content[len(content) - 1]
            row = info.line
            column = info.current_token
            mystr = info.str_at_line
            print("SyntaxError (line", str(row) + ", column",
                  str(column) + "): token of kind TokenKind.NEWLINE not found.")
            print(">>>" + mystr)
            print(">>>", end="")
            for i in range(0, column - 1):
                print("-", end="")
            print("^")
            exit(0)

    def check_lsqaure_error(self):
        if not self.check(TokenKind.LSQUAREBRACKET):
            # [row, column, mystr] = self.lexer.return_row_column()
            self.lexer.self_finish()
            content = self.lexer.tokenizer.content
            info = content[len(content) - 1]
            row = info.line
            column = info.current_token
            mystr = info.str_at_line
            print("SyntaxError (line", str(row) + ", column",
                  str(column) + "): token of kind TokenKind.LSQUAREBRACKET not found.")
            print(">>>" + mystr)
            print(">>>", end="")
            for i in range(0, column - 1):
                print("-", end="")
            print("^")
            exit(0)

    def check_rsqaure_error(self):
        if not self.check(TokenKind.RSQUAREBRACKET):
            # [row, column, mystr] = self.lexer.return_row_column()
            self.lexer.self_finish()
            content = self.lexer.tokenizer.content
            info = content[len(content) - 1]
            row = info.line
            column = info.current_token
            mystr = info.str_at_line
            print("SyntaxError (line", str(row) + ", column",
                  str(column) + "): token of kind TokenKind.RSQUAREBRACKET not found.")
            print(">>>" + mystr)
            print(">>>", end="")
            for i in range(0, column - 1):
                print("-", end="")
            print("^")
            exit(0)

    def check_unmatched_parenthesis(self):
        if self.check(TokenKind.RROUNDBRACKET):
            # [row, column, mystr] = self.lexer.return_row_column()
            self.lexer.self_finish()
            content = self.lexer.tokenizer.content
            info = content[len(content) - 1]
            row = info.line
            column = info.current_token
            mystr = info.str_at_line
            print("SyntaxError (line", str(row) + ", column", str(column) + "): unmatched ')'.")
            print(">>>" + mystr)
            print(">>>", end="")
            for i in range(0, column - 1):
                print("-", end="")
            print("^")
            exit(0)

    def check_block_indent_error(self):
        if not self.check(TokenKind.INDENT):
            # [row, column, mystr] = self.lexer.return_row_column()
            self.lexer.self_finish()
            content = self.lexer.tokenizer.content
            info = content[len(content) - 1]
            row = info.line
            column = info.current_token
            mystr = info.str_at_line
            print("SyntaxError (line", str(row) + ", column",
                  str(column) + "): expected at least one indented statement in block.")
            print(">>>" + mystr)
            print(">>>", end="")
            for i in range(0, column - 1):
                print("-", end="")
            print("^")
            exit(0)

    def check_func_indent_error(self):
        if not self.check(TokenKind.INDENT):
            # [row, column, mystr] = self.lexer.return_row_column()
            self.lexer.self_finish()
            content = self.lexer.tokenizer.content
            info = content[len(content) - 1]
            row = info.line
            column = info.current_token
            mystr = info.str_at_line
            print("SyntaxError (line", str(row) + ", column",
                  str(column) + "): expected at least one indented statement in function.")
            print(">>>" + mystr)
            print(">>>", end="")
            for i in range(0, column - 1):
                print("-", end="")
            print("^")
            exit(0)

    def check_unexpected_indent_error(self):
        if self.check(TokenKind.INDENT):
            # [row, column, mystr] = self.lexer.return_row_column()
            self.lexer.self_finish()
            content = self.lexer.tokenizer.content
            info = content[len(content) - 1]
            row = info.line
            column = info.current_token
            mystr = info.str_at_line

            indent = 0
            find_indent = False
            for i in range(len(content) - 2, -1, -1):
                if find_indent:
                    break
                current_line_str = content[i].str_at_line
                for j in range(len(current_line_str)):
                    if current_line_str[j].isspace():
                        if current_line_str[j] == "\t":
                            indent += 8
                        elif current_line_str[j] == "\n" or current_line_str[j] == "\r":
                            indent = 0
                            break
                        else:
                            indent += 1
                    elif current_line_str[j] == '#':
                        indent = 0
                        break
                    elif not current_line_str[j]:
                        indent = 0
                        break
                    else:
                        find_indent = True
                        break

            column = indent + 1

            print("SyntaxError (line", str(row) + ", column", str(column) + "): Unexpected indentation.")
            print(">>>" + mystr)
            print(">>>", end="")
            for i in range(0, column - 1):
                print("-", end="")
            print("^")

            exit(0)

    def check_variable_declaration_error(self):
        if self.check(TokenKind.COLON):
            # [row, column, mystr] = self.lexer.return_row_column()
            self.lexer.self_finish()
            content = self.lexer.tokenizer.content
            info = content[len(content) - 1]
            row = info.line
            column = info.current_token
            mystr = info.str_at_line
            print("SyntaxError (line", str(row) + ", column",
                  str(column) + "): Variable declaration after non-declaration statement.")
            print(">>>" + mystr)
            print(">>>", end="")
            for i in range(0, column - 1):
                print("-", end="")
            print("^")
            exit(0)

    def check_no_statement_error(self):
        self.lexer.self_finish()
        content = self.lexer.tokenizer.content
        info = content[len(content) - 1]
        row = info.line
        column = info.current_token
        mystr = info.str_at_line
        print("SyntaxError (line", str(row) + ", column",
              str(column) + "): expected at least one indented statement in function.")
        print(">>>" + mystr)
        print(">>>", end="")
        for i in range(0, column - 1):
            print("-", end="")
        print("^")
        exit(0)
