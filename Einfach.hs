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

  times :: Parser BinOp
  times = char '*' >> return Multiplication

  divide :: Parser BinOp
  divide = char '/' >> return Division

  operator :: Parser BinOp
  operator = plus <|> minus <|> times <|> divide

  assignment :: Parser Statement
  assignment = do i <- many1 alphaNum
                  _ <- char ':'
                  spaces
                  e <- expr
                  return $ Assignment i e

  identExpr :: Parser Expr
  identExpr = do s1 <- letter
                 s2 <- many1 alphaNum
                 return $ IdentExpr $ s1 : s2

  numExpr :: Parser Expr
  numExpr = do ds <- many1 digit
               return $ NumExpr ds

  opExpr :: Parser Expr
  opExpr = do left <- numExpr <|> identExpr
              op <- operator
              right <- numExpr <|> identExpr
              return $ OpExpr left op right

  eseqExpr :: Parser Expr
  eseqExpr = do char '(' >> spaces
                s <- statement
                spaces >> char ',' >> spaces
                e <- expr
                _ <- spaces >> char ')'
                return $ EseqExpr s e

  statement :: Parser Statement
  statement = undefined

  expr :: Parser Expr
  expr = identExpr <|> opExpr <|> numExpr

  main :: IO ()
  main = case parse assignment "example" inputText of
              Left  err -> print err
              Right res -> putStrLn $ "I parsed: '" ++ show res ++ "'"

  inputText :: String
  inputText = "a: 5+3"

  realTest :: String
  realTest = "a := 5 + 3; b := (print(a, a - 1), 10 * a); print(b)"
