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
            return token

        token = self.lexer.peek()
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
        defs: List[Operation] = []
        stmts: List[Operation] = []
        if self.check(TokenKind.IDENTIFIER) or self.check(TokenKind.DEF):

            defs = self.parse_multi_opt_var_or_func_def()
        if self.is_stmt_first_set():
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
        if self.check(TokenKind.IDENTIFIER) or self.check(TokenKind.DEF):
            return self.parse_multi_opt_var_or_func_def_helper()

    def parse_multi_opt_var_or_func_def_helper(self) -> List[Operation]:
        operation = self.parse_var_or_func()
        if self.check(TokenKind.IDENTIFIER) or self.check(TokenKind.DEF):
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
        return self.parse_multi_stmt_helper()

    def parse_multi_stmt_helper(self) -> List[Operation]:
        if self.is_stmt_first_set():
            stmt = self.parse_stmt()
            lists = self.parse_multi_stmt_helper()
            lists.insert(0, stmt)
            return lists
        return []


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

        self.match(TokenKind.LROUNDBRACKET)

        # Function parameters
        parameters: List[Operation] = []
        if self.check(TokenKind.IDENTIFIER):
            parameters = self.parse_argument()

        self.match(TokenKind.RROUNDBRACKET)

        return_type: ast.TypeName
        # Return type: default is <None>.
        if self.check(TokenKind.RARROW):
            return_type = self.parse_return_type_opt()
        else:
            return_type = ast.TypeName('<None>')

        self.match(TokenKind.COLON)

        self.match(TokenKind.NEWLINE)
        self.match(TokenKind.INDENT)

        func_body = []
        if self.check(TokenKind.GLOBAL) or self.check(TokenKind.NONLOCAL) or self.check(TokenKind.IDENTIFIER):
            func_body: List[Operation] = self.parse_func_body()

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

    def parse_stmt_seq(self) -> List[Operation]:
        """Parse a sequence of statements.

        stmt_seq := stmt stmt_seq

        :return: list of Operations
        """
        stmt_seq: List[Operation] = []
        while self.is_stmt_first_set():
            stmt_op = self.parse_stmt()
            stmt_seq.append(stmt_op)
        return stmt_seq

    def parse_stmt(self) -> Operation:
        """Parse a statement.

        stmt := simple_stmt NEWLINE

        The above definition is incomplete.

        TODO: Not fully implemented.
        :return: Statement as operation
        """
        stmt: Operation
        if self.check(TokenKind.PASS) or self.is_expr_first_set() or self.check(TokenKind.RETURN):
            simple_stmt = self.parse_simple_stmt()
            self.match(TokenKind.NEWLINE)
            return simple_stmt
        elif self.check(TokenKind.IF):
            pass
        elif self.check(TokenKind.WHILE):
            pass
        elif self.check(TokenKind.FOR):
            pass
        return stmt

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
            return self.parse_expr()
        elif self.check(TokenKind.RETURN):
            self.match(TokenKind.RETURN)
            if self.is_expr_first_set():
                operation = self.parse_expr()
                return ast.Return(operation)
            return ast.Return()
        elif self.check(TokenKind.ASSIGN):
            pass
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
        stmt = self.parse_stmt_pos()

        return assignment

    def parse_all_variables_assignment(self) -> List[Operation]:
        if self.check(TokenKind.GLOBAL) or self.check(TokenKind.NONLOCAL) or self.check(TokenKind.IDENTIFIER):
            return self.parse_all_variables_assignment_helper()

    def parse_all_variables_assignment_helper(self) -> List[Operation]:
        operation = self.parse_global_or_nonlocal_or_var()
        if self.check(TokenKind.GLOBAL) or self.check(TokenKind.NONLOCAL) or self.check(TokenKind.IDENTIFIER):
            list_operation = self.parse_all_variables_assignment_helper()
            list_operation.append(operation)
            return list_operation
        else:
            lists: List[Operation] = [operation]
            return lists

    def parse_stmt_pos(self):
        pass

    def parse_global_or_nonlocal_or_var(self) -> Operation:
        if self.check(TokenKind.GLOBAL):
            global_decl = self.parse_global_decl()
            return global_decl
        if self.check(TokenKind.NONLOCAL):
            non_local = self.parse_nonlocal_decl()
            return non_local
        if self.check(TokenKind.NONLOCAL):
            var_def = self.parse_var()
            return var_def

    def parse_global_decl(self) -> Operation:
        self.match(TokenKind.GLOBAL)
        id = self.match(TokenKind.IDENTIFIER)
        self.match(TokenKind.NEWLINE)
        return ast.GlobalDecl(id.value)

    def parse_nonlocal_decl(self) -> Operation:
        self.match(TokenKind.NONLOCAL)
        id = self.match(TokenKind.IDENTIFIER)
        self.match(TokenKind.NEWLINE)

        return ast.NonLocalDecl(id.value)

    def parse_var(self) -> Operation:
        typed_var = self.parse_typed_var()
        self.match(TokenKind.ASSIGN)
        literal = self.parse_literal()
        self.match(TokenKind.NEWLINE)
        return ast.VarDef(typed_var, literal)

    def parse_typed_var(self) -> ast.TypedVar:
        id = self.match(TokenKind.IDENTIFIER)
        self.match(TokenKind.COLON)
        type_name = self.parse_type()

        return ast.TypedVar(id.value, type_name)

    def parse_type(self) -> ast.TypeName:
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
            self.match(TokenKind.RSQUAREBRACKET)
            return type_new
        else:
            exit(0)

        return ast.TypeName(type_token.value)

    def parse_argument(self) -> List[Operation]:
        typed_var = self.parse_typed_var()
        lists = self.parse_multi_typed_var()
        lists.insert(0, typed_var)
        return lists

    def parse_multi_typed_var(self) -> List[Operation]:
        return self.parse_multi_typed_var_helper()

    def parse_multi_typed_var_helper(self) -> List[Operation]:
        self.match(TokenKind.COMMA)
        typed_var = self.parse_typed_var()
        if self.check(TokenKind.IDENTIFIER):
            lists = self.parse_multi_typed_var_helper()
            lists.append(typed_var)
            return lists
        else:
            lists = [typed_var]
            return lists

    def parse_return_type_opt(self) -> ast.TypeName:
        self.match(TokenKind.RARROW)
        return_type = self.parse_type()
        return return_type

    def parse_index_expr(self):
        value = self.parse_cexpr()
        self.match(TokenKind.LSQUAREBRACKET)
        index = self.parse_expr()
        self.match(TokenKind.RSQUAREBRACKET)
        return ast.IndexExpr(value, index)

    def parse_cexpr(self) -> Operation:
        pass

    def parse_expr(self) -> Operation:
        if self.check(TokenKind.IDENTIFIER):
            return ast.ExprName(self.match(TokenKind.IDENTIFIER).value)
        return self.parse_literal()




