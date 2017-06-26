
from pygments.lexer import *
from pygments.token import *
import re

class EssenceLexer(RegexLexer):
   
    name = 'Essence'
    # flags = re.MULTILINE | re.DOTALL

    ascii = ('NUL', 'SOH', '[SE]TX', 'EOT', 'ENQ', 'ACK',
             'BEL', 'BS', 'HT', 'LF', 'VT', 'FF', 'CR', 'S[OI]', 'DLE',
             'DC[1-4]', 'NAK', 'SYN', 'ETB', 'CAN',
             'EM', 'SUB', 'ESC', '[FGRU]S', 'SP', 'DEL')

    reserved =  ( 'find'
                , 'given'
                , 'letting'
                , 'be'
                , 'domain'
                , 'set'
                , 'of'
                , 'int'
                )

    tokens = {
        'root': [
            (r'\b(%s)(?!\')\b' % '|'.join(reserved), Keyword.Type),
            (r'list', Keyword),

            (r'(:=|\||\*|\(|\)|\{|\})', Operator),

            # Strings
            (r"'", String.Char, 'character'),
            (r'"', String, 'string'),
            (r'[^\s(){}]+', Text),
            (r'\s+?', Text),  # Whitespace
        ],
        'character': [
            # Allows multi-chars, incorrectly.
            (r"[^\\']", String.Char),
            (r"\\", String.Escape, 'escape'),
            ("'", String.Char, '#pop'),
        ],
        'string': [
            (r'[^\\"]+', String),
            (r"\\", String.Escape, 'escape'),
            ('"', String, '#pop'),
        ],
        'escape': [
            (r'[abfnrtv"\'&\\]', String.Escape, '#pop'),
            (r'\^[][A-Z@^_]', String.Escape, '#pop'),
            ('|'.join(ascii), String.Escape, '#pop'),
            (r'o[0-7]+', String.Escape, '#pop'),
            (r'x[\da-fA-F]+', String.Escape, '#pop'),
            (r'\d+', String.Escape, '#pop'),
            (r'\s+\\', String.Escape, '#pop')
        ],
    }







# ALL TOKEN TYPES
#
# Keyword
# Keyword.Constant
# Keyword.Declaration
# Keyword.Namespace
# Keyword.Pseudo
# Keyword.Reserved
# Keyword.Type
# Name
# Name.Attribute
# Name.Builtin
# Name.Builtin.Pseudo
# Name.Class
# Name.Constant
# Name.Decorator
# Name.Entity
# Name.Exception
# Name.Function
# Name.Label
# Name.Namespace
# Name.Other
# Name.Tag
# Name.Variable
# Name.Variable.Class
# Name.Variable.Global
# Name.Variable.Instance
# Literal
# Literal.Date
# String
# String.Backtick
# String.Char
# String.Doc
# String.Double
# String.Escape
# String.Heredoc
# String.Interpol
# String.Other
# String.Regex
# String.Single
# String.Symbol
# Number
# Number.Bin
# Number.Float
# Number.Hex
# Number.Integer
# Number.Integer.Long
# Number.Oct
# Operator
# Operator.Word
# Punctuation
# Comment
# Comment.Hashbang
# Comment.Multiline
# Comment.Preproc
# Comment.Single
# Comment.Special
# Generic
# Generic.Deleted
# Generic.Emph
# Generic.Error
# Generic.Heading
# Generic.Inserted
# Generic.Output
# Generic.Prompt
# Generic.Strong
# Generic.Subheading
# Generic.Traceback
