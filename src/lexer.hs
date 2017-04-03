module Lexer
( identifier,
  reserved,
  reservedOp,
  parens,
  integer,
  semi,
  whiteSpace
) where
import Structure
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

-- Language Definition
languageDef =
  emptyDef { Token.commentStart = "/*",
             Token.commentEnd = "*/",
             Token.commentLine = "//",
             Token.identStart = letter <|> char '_',
             Token.identLetter = alphaNum <|> char '_',
             Token.reservedOpNames = ["+", "-", "*", "/", "<-", "is", "=", "{",
                                      "}", ",", ".", "<", ">", "=="
                                     ],
             Token.reservedNames = ["domain", "values", "ranges", "true", "is",
                                    "false", "not", "and", "or", "if", "then",
                                    "else"
                                   ]
           }

-- Lexer
lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
parens     = Token.parens     lexer -- parses surrounding parenthesis:
                                    --   parens p
                                    -- takes care of the parenthesis and
                                    -- uses p to parse what's inside them
integer    = Token.integer    lexer -- parses an integer
semi       = Token.semi       lexer -- parses a semicolon
whiteSpace = Token.whiteSpace lexer -- parses whitespace
