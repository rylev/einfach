module MyInterpreter where

  import Text.Parsec
  import Text.Parsec.String

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

  multiplication :: Parser BinOp
  multiplication = char '*' >> return Multiplication

  division :: Parser BinOp
  division = char '/' >> return Division

  assignment :: Parser Statement
  assignment = do i <- many1 alphaNum
                  _ <- char ':'
                  spaces
                  e <- identExpr
                  return $ Assignment i e

  identExpr :: Parser Expr
  identExpr = do s1 <- letter
                 s2 <- many1 alphaNum
                 return $ IdentExpr $ s1 : s2

  main :: IO ()
  main = case parse assignment "example" inputText of
              Left  err -> print err
              Right res -> putStrLn $ "I parsed: '" ++ show res ++ "'"

  inputText :: String
  inputText = "a: lol"

  realTest :: String
  realTest = "a := 5 + 3; b := (print(a, a - 1), 10 * a); print(b)"
