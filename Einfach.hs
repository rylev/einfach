module MyInterpreter where

  import Text.Parsec
  import Text.Parsec.String
  import qualified Text.Parsec.Token as P
  import Data.Functor.Identity
  import Control.Monad

  lexer :: P.GenTokenParser String u Identity
  lexer = P.makeTokenParser einfachDef

  einfachDef :: P.LanguageDef s
  einfachDef = P.LanguageDef {
    P.commentStart = "#--",
    P.commentEnd = "--#",
    P.commentLine = "#",
    P.nestedComments = False,
    P.identStart = letter <|> char '_',
    P.identLetter = alphaNum <|> char '_',
    P.opStart = oneOf ":!#$%&*+./<=>?@\\^|-~",
    P.opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~",
    P.reservedNames = [],
    P.reservedOpNames = [],
    P.caseSensitive = True
  }

  whiteSpace :: Parser ()
  whiteSpace = P.whiteSpace lexer

  mainParser :: Parser [Statement]
  mainParser = do whiteSpace
                  ss <- statements
                  eof
                  return ss

  type Ident = String

  data BinOp = Plus | Minus | Multiplication | Division deriving Show

  data Statement = CompoundStatement Statement Statement
                 | Assignment Ident Expr
                 | PrintStatement [Expr]
                 deriving Show

  data Expr = IdentExpr Ident
            | NumExpr Ident
            | OpExpr Expr BinOp Expr
            | EseqExpr Statement Expr
            deriving Show

  plus :: Parser BinOp
  plus = char '+' >> return Plus

  minus :: Parser BinOp
  minus = char '-' >> return Minus

  times :: Parser BinOp
  times = char '*' >> return Multiplication

  divide :: Parser BinOp
  divide = char '/' >> return Division

  oper :: Parser BinOp
  oper = plus <|> minus <|> times <|> divide

  ident :: Parser String
  ident = do s1 <- letter
             s2 <- many alphaNum
             return $ s1 : s2

  assignment :: Parser Statement
  assignment = do i <- ident
                  spaces >> string "=" >> spaces
                  e <- expr
                  return $ Assignment i e

  printStatement :: Parser Statement
  printStatement = do e <- string "print" >> char '(' >> expr
                      _ <- char ')'
                      return $ PrintStatement [e]

  statement :: Parser Statement
  statement = try assignment <|> printStatement

  statements :: Parser [Statement]
  statements = sepBy1 statement statementSep
               where statementSep = do reglarSpaces
                                       eol <|> void (string ";")
                                       reglarSpaces


  reglarSpaces :: Parser ()
  reglarSpaces = void . many $ oneOf " \t\f\v"

  eol :: Parser ()
  eol = void eolParser
        where eolParser = try (string "\n\r")
                          <|> try (string "\r\n")
                          <|> string "\n"
                          <|> string "\r"
                          <?> "eol"

  identExpr :: Parser Expr
  identExpr = do s <- ident
                 return $ IdentExpr s

  numExpr :: Parser Expr
  numExpr = do ds <- many1 digit
               return $ NumExpr ds

  opExpr :: Parser Expr
  opExpr = do left <- numExpr <|> identExpr
              spaces
              op <- oper
              spaces
              right <- numExpr <|> identExpr
              return $ OpExpr left op right

  eseqExpr :: Parser Expr
  eseqExpr = do char '(' >> spaces
                s <- statement
                spaces >> char ',' >> spaces
                e <- expr
                _ <- spaces >> char ')'
                return $ EseqExpr s e

  expr :: Parser Expr
  expr = eseqExpr <|> try opExpr <|> identExpr <|> numExpr

  main :: IO ()
  main = case parse mainParser  "example" inputText of
              Left  err -> print err
              Right res -> putStrLn $ "I parsed: '" ++ show res ++ "'"

  inputText :: String
  inputText = "a = 5 + 3    \n b = (print(a), 10 * a); print(b)"
