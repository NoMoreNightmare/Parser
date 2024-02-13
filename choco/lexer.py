from dataclasses import dataclass
from enum import Enum, auto
from io import TextIOBase
from typing import Any, List, Optional, Union


class TokenKind(Enum):
    EOF = auto()

    # Newline
    NEWLINE = auto()

    # Indentation
    INDENT = auto()
    DEDENT = auto()

    # Commands
    CLASS = auto()
    DEF = auto()
    GLOBAL = auto()
    NONLOCAL = auto()
    PASS = auto()
    RETURN = auto()

    # Primary
    IDENTIFIER = auto()
    INTEGER = auto()
    STRING = auto()

    # Control-flow
    IF = auto()
    ELIF = auto()
    ELSE = auto()
    WHILE = auto()
    FOR = auto()
    IN = auto()

    # Symbols
    PLUS = auto()
    MINUS = auto()
    MUL = auto()
    DIV = auto()
    MOD = auto()
    ASSIGN = auto()
    LROUNDBRACKET = auto()
    RROUNDBRACKET = auto()
    COLON = auto()
    LSQUAREBRACKET = auto()
    RSQUAREBRACKET = auto()
    COMMA = auto()
    RARROW = auto()

    # Comparison Operators
    EQ = auto()
    NE = auto()
    LT = auto()
    GT = auto()
    LE = auto()
    GE = auto()
    IS = auto()

    # VALUES
    NONE = auto()
    TRUE = auto()
    FALSE = auto()

    # Logical operators
    OR = auto()
    AND = auto()
    NOT = auto()

    # TYPES
    OBJECT = auto()
    INT = auto()
    BOOL = auto()
    STR = auto()


@dataclass
class Token:
    kind: TokenKind
    value: Any = None

    def __repr__(self) -> str:
        if self.kind is TokenKind.STRING:
            # Get the string with escaped characters.
            str2 = (
                self.value.replace("\\", "\\\\")
                .replace("\t", "\\t")
                .replace("\r", "\\r")
                .replace("\n", "\\n")
                .replace('"', '\\"')
            )
            return self.kind.name + ":" + str2
        else:
            return self.kind.name + (
                (":" + str(self.value)) if self.value is not None else "")


class Scanner:

    def __init__(self, stream: TextIOBase):
        """Create a new scanner.

        The scanner's input is a stream derived from a string or a file.

        # Lexing a string
        s = io.StringIO("1 + 2")
        Scanner(s)

        # Lexing a file
        s = open("file.choc")
        Scanner(s)

        :param stream: The stream of characters to be lexed.
        """
        self.stream: TextIOBase = stream
        self.buffer: Optional[str] = None  # A buffer of one character.

    def peek(self) -> str:
        """Return the next character from input without consuming it.

        :return: The next character in the input stream or None at the end of the stream.
        """
        # A buffer of one character is used to store the last scanned character.
        # If the buffer is empty, it is filled with the next character from the input.
        if not self.buffer:
            self.buffer = self.consume()
        return self.buffer

    def consume(self):
        """Consume and return the next character from input.

        :return: The next character in the input stream or None at the end of the stream.
        """
        # If the buffer is full, we empty the buffer and return the character it contains.
        if self.buffer:
            c = self.buffer
            self.buffer = None
            return c
        return self.stream.read(1)


class Tokenizer:

    def __init__(self, scanner: Scanner):
        self.scanner = scanner
        self.buffer: List[Token] = []  # A buffer of tokens
        self.is_new_line = True
        self.is_logical_line = False
        self.line_indent_lvl = 0  # How "far" we are inside the line, i.e. what column.
        # Resets after every end-of-line sequence.
        self.indent_stack = [0]

        self.line = 1
        self.column = 1
        self.token_start = 0
        self.mystr = ''

    def peek(self, k: int = 1) -> Union[Token, List[Token]]:
        """Peeks through the next `k` number of tokens.

        This functions looks ahead the next `k` number of tokens,
        and returns them as a list.
        It uses a FIFO buffer to store tokens temporarily.
        :param k: number of tokens
        :return: one token or a list of tokens
        """
        if not self.buffer:
            self.buffer = [self.consume()]

        # Fill the buffer up to `k` tokens, if needed.
        buffer_size = len(self.buffer)
        if buffer_size < k:
            for _ in list(range(k - buffer_size)):
                self.buffer.append(self.consume(keep_buffer=True))

        # If you need only one token, return it as an element,
        # not as a list with one element.
        if k == 1:
            return self.buffer[0]

        return self.buffer[0:k]

    def consume(self, keep_buffer: bool = False) -> Token:
        """Consumes one token and implements peeking through the next one.

        If we want to only peek and not consume, we set the `keep_buffer` flag.
        This argument is passed to additional calls to `next`.
        The idea is that we can peek through more than one tokens (by keeping them in the buffer),
        but we consume tokens only one by one.
        :param keep_buffer: whether to keep the buffer intact, or consume a token from it
        :return: one token
        """
        if self.buffer and not keep_buffer:
            c = self.buffer[0]
            self.buffer = self.buffer[1:]
            return c

        c = self.scanner.peek()

        while True:
            if c.isspace():
                # Tabs are replaced from left to right by one to eight spaces.
                # The total number of spaces up to and including the replacement should be a multiple of eight.
                if c == "\t":
                    self.column += 8
                    self.mystr += c
                    if self.is_new_line:  # We only care about padding at the beginning of line.
                        self.line_indent_lvl += 8 - self.line_indent_lvl % 8
                elif c == "\n":  # line feed handling
                    self.line_indent_lvl = 0
                    self.is_new_line = True

                    self.line = self.line + 1
                    self.column = 1
                    self.mystr = ''

                    if self.is_logical_line:
                        self.is_logical_line = False
                        self.scanner.consume()
                        return Token(TokenKind.NEWLINE, None)
                elif c == "\r":  # carriage return handling
                    self.line_indent_lvl = 0
                    self.is_new_line = True

                    self.line = self.line + 1
                    self.column = 1
                    self.mystr = ''

                    if self.is_logical_line:
                        self.is_logical_line = False
                        self.scanner.consume()
                        return Token(TokenKind.NEWLINE, None)
                else:  # Handle the rest whitespaces

                    self.column += 1
                    self.mystr += c

                    if self.is_new_line:
                        self.line_indent_lvl += 1
                # Consume whitespace
                self.scanner.consume()
                return self.consume(keep_buffer)
            # One line comments
            # get_char() returns None in the case of EOF.
            elif c == '#':
                self.scanner.consume()
                c = self.scanner.peek()

                self.column += 1
                self.mystr += c

                while c and c != "\n" and c != "\r":
                    self.scanner.consume()
                    c = self.scanner.peek()

                    self.column += 1
                    self.mystr += c

                continue
            # Indentation
            elif c and not c.isspace() and c != "#" and self.is_new_line:
                # OK, we are in a logical line now (at least one token that is not whitespace or comment).
                self.is_logical_line = True
                if (
                        self.line_indent_lvl > self.indent_stack[-1]
                ):  # New indentation level
                    # Push the indentation level to the stack.
                    self.indent_stack.append(self.line_indent_lvl)
                    # Return indent token.
                    # Do not consume any character (this will happen in the next call of get_token()).
                    return Token(TokenKind.INDENT, None)
                elif (
                        self.line_indent_lvl < self.indent_stack[-1]
                ):  # Previous indentation level is (probably) closing.
                    try:
                        self.indent_stack.index(self.line_indent_lvl)
                        # Pop the last of the indentation levels that are higher.
                        self.indent_stack = self.indent_stack[0:-1]
                        # Return dedent token.
                        # Do not consume any character (this will happen in a next call of get_token()).
                        return Token(TokenKind.DEDENT, None)
                    except ValueError:
                        print("Indentation error: mismatched blocks.")
                        exit(1)
                self.is_new_line = False
            elif c == "+":
                self.scanner.consume()

                self.token_start = self.column
                self.column += 1
                self.mystr += c

                return Token(TokenKind.PLUS, "+")
            elif c == "-":
                self.scanner.consume()
                c += self.scanner.peek()
                if c == "->":
                    self.scanner.consume()

                    self.token_start = self.column
                    self.column += 2
                    self.mystr += c

                    return Token(TokenKind.RARROW, "->")
                else:

                    self.token_start = self.column
                    self.column += 1
                    self.mystr += "-"

                    return Token(TokenKind.MINUS, "-")
            elif c == "*":

                self.token_start = self.column
                self.column += 1
                self.mystr += c


                self.scanner.consume()
                return Token(TokenKind.MUL, "*")
            elif c == "%":

                self.token_start = self.column
                self.column += 1
                self.mystr += c

                self.scanner.consume()
                return Token(TokenKind.MOD, "%")
            elif c == "/":

                self.scanner.consume()
                c += self.scanner.peek()
                if c == "//":

                    self.token_start = self.column
                    self.column += 2
                    self.mystr += c

                    self.scanner.consume()
                    return Token(TokenKind.DIV, "//")
                else:
                    self.token_start = self.column
                    self.column += 1
                    self.mystr += "/"

                    raise Exception("Unknown lexeme: {}".format(c))
            elif c == "=":
                self.scanner.consume()
                c += self.scanner.peek()
                if c == "==":

                    self.token_start = self.column
                    self.column += 2
                    self.mystr += c

                    self.scanner.consume()
                    return Token(TokenKind.EQ, "==")
                else:

                    self.token_start = self.column
                    self.column += 1
                    self.mystr += "="

                    return Token(TokenKind.ASSIGN, "=")
            elif c == "!":
                self.scanner.consume()
                c += self.scanner.peek()
                if c == "!=":

                    self.token_start = self.column
                    self.column += 2
                    self.mystr += c

                    self.scanner.consume()
                    return Token(TokenKind.NE, "!=")
                else:
                    self.token_start = self.column
                    self.column += 1
                    self.mystr += "!"

                    raise Exception("Unknown lexeme: {}".format(c))
            elif c == "<":

                self.scanner.consume()
                c += self.scanner.peek()
                if c == "<=":

                    self.token_start = self.column
                    self.column += 2
                    self.mystr += c

                    self.scanner.consume()
                    return Token(TokenKind.LE, "<=")
                else:

                    self.token_start = self.column
                    self.column += 1
                    self.mystr += "<"

                    return Token(TokenKind.LT, "<")
            elif c == ">":
                self.scanner.consume()
                c += self.scanner.peek()
                if c == ">=":

                    self.token_start = self.column
                    self.column += 2
                    self.mystr += c

                    self.scanner.consume()
                    return Token(TokenKind.GE, ">=")
                else:

                    self.token_start = self.column
                    self.column += 1
                    self.mystr += ">"

                    return Token(TokenKind.GT, ">")
            elif c == "(":

                self.token_start = self.column
                self.column += 1
                self.mystr += c

                self.scanner.consume()
                return Token(TokenKind.LROUNDBRACKET, "(")
            elif c == ")":

                self.token_start = self.column
                self.column += 1
                self.mystr += c

                self.scanner.consume()
                return Token(TokenKind.RROUNDBRACKET, ")")
            elif c == ":":
                self.token_start = self.column
                self.column += 1
                self.mystr += c

                self.scanner.consume()
                return Token(TokenKind.COLON, ":")
            elif c == "[":
                self.token_start = self.column
                self.column += 1
                self.mystr += c

                self.scanner.consume()
                return Token(TokenKind.LSQUAREBRACKET, "[")
            elif c == "]":
                self.token_start = self.column
                self.column += 1
                self.mystr += c

                self.scanner.consume()
                return Token(TokenKind.RSQUAREBRACKET, "]")
            elif c == ",":
                self.token_start = self.column
                self.column += 1
                self.mystr += c

                self.scanner.consume()
                return Token(TokenKind.COMMA, ",")
            # Identifier: [a-zA-Z_][a-zA-Z0-9_]*
            elif c.isalpha() or c == "_":
                self.token_start = self.column
                self.column += 1
                self.mystr += c

                name = self.scanner.consume()
                c = self.scanner.peek()
                while c.isalnum() or c == "_":

                    self.column += 1
                    self.mystr += c

                    name += self.scanner.consume()
                    c = self.scanner.peek()

                if name == "class":
                    return Token(TokenKind.CLASS, "class")
                if name == "def":
                    return Token(TokenKind.DEF, "def")
                if name == "global":
                    return Token(TokenKind.GLOBAL, "global")
                if name == "nonlocal":
                    return Token(TokenKind.NONLOCAL, "nonlocal")
                if name == "if":
                    return Token(TokenKind.IF, "if")
                if name == "elif":
                    return Token(TokenKind.ELIF, "elif")
                if name == "else":
                    return Token(TokenKind.ELSE, "else")
                if name == "while":
                    return Token(TokenKind.WHILE, "while")
                if name == "for":
                    return Token(TokenKind.FOR, "for")
                if name == "in":
                    return Token(TokenKind.IN, "in")
                if name == "None":
                    return Token(TokenKind.NONE, "None")
                if name == "True":
                    return Token(TokenKind.TRUE, "True")
                if name == "False":
                    return Token(TokenKind.FALSE, "False")
                if name == "pass":
                    return Token(TokenKind.PASS, "pass")
                if name == "or":
                    return Token(TokenKind.OR, "or")
                if name == "and":
                    return Token(TokenKind.AND, "and")
                if name == "not":
                    return Token(TokenKind.NOT, "not")
                if name == "is":
                    return Token(TokenKind.IS, "is")
                if name == "object":
                    return Token(TokenKind.OBJECT, "object")
                if name == "int":
                    return Token(TokenKind.INT, "int")
                if name == "bool":
                    return Token(TokenKind.BOOL, "bool")
                if name == "str":
                    return Token(TokenKind.STR, "str")
                if name == "return":
                    return Token(TokenKind.RETURN, "return")

                return Token(TokenKind.IDENTIFIER, name)
            # Number: [0-9]+
            elif c.isdigit():
                self.token_start = self.column
                self.column += 1
                self.mystr += c

                value = self.scanner.consume()
                while self.scanner.peek().isnumeric():
                    self.column += 1
                    self.mystr += c
                    value += self.scanner.consume()
                return Token(TokenKind.INTEGER, int(value))
            # String
            elif c == '"':
                string: str = ""

                self.token_start = self.column
                self.column += 1
                self.mystr += c

                self.scanner.consume()
                c = self.scanner.peek()
                while c != '"':
                    if 32 <= ord(c) <= 126:  # ASCII limits accepted
                        if c != "\\":
                            self.token_start = self.column
                            self.column += 1
                            self.mystr += c
                            string += self.scanner.consume()
                        # Handle escape characters
                        else:
                            self.scanner.consume()  # Consume '\\'
                            c = self.scanner.peek()
                            if c == "n":
                                string += "\n"

                                self.column += 1
                                self.mystr += "\n"
                            elif c == "t":

                                self.column += 1
                                self.mystr += "\t"
                                string += "\t"
                            elif c == '"':
                                string += '"'

                                self.column += 1
                                self.mystr += '"'

                            elif c == "\\":
                                string += "\\"

                                self.column += 1
                                self.mystr += "\\"
                            else:
                                print('Error: "\\{}" not recognized'.format(c))
                                exit(1)
                            self.scanner.consume()  # Consume escaped character
                    else:
                        print("Error: Unknown ASCII number {}".format(ord(c)))
                        exit(1)
                    c = self.scanner.peek()
                self.column += 1
                self.mystr += '"'
                self.scanner.consume()
                return Token(TokenKind.STRING, string)
            # End of file
            elif not c:
                # The end of input also serves as an implicit terminator of the physical line.
                # For a logical line emit a newline token.
                self.token_start = self.column
                if self.is_logical_line:
                    self.is_logical_line = False
                    return Token(TokenKind.NEWLINE, None)
                if (
                        self.indent_stack[-1] > 0
                ):  # A dedent token is generated for the rest non-zero numbers on the stack.
                    self.indent_stack = self.indent_stack[0:-1]
                    return Token(TokenKind.DEDENT, None)
                return Token(TokenKind.EOF, None)
            else:
                raise Exception("Invalid character detected: '" + c + "'")


class Lexer:
    def __init__(self, stream: TextIOBase):
        scanner = Scanner(stream)
        self.tokenizer = Tokenizer(scanner)

    def peek(self, k: int = 1) -> Union[Token, List[Token]]:
        return self.tokenizer.peek(k)

    def consume(self) -> Token:
        return self.tokenizer.consume()

    def return_row_column(self) -> List[Union[int, str]]:
        mystr = self.tokenizer.mystr
        c = self.tokenizer.scanner.peek()
        while c != "\n" and c != "\r" and c:

            mystr += c
            self.tokenizer.scanner.consume()

            c = self.tokenizer.scanner.peek()
        return [self.tokenizer.line, self.tokenizer.token_start, mystr]
