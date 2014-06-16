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

  identifier :: Parser String
  identifier = do s1 <- letter
                  s2 <- many alphaNum
                  return $ s1 : s2

  assignment :: Parser Statement
  assignment = do i <- identifier
                  spaces >> string ":=" >> spaces
                  e <- expr
                  return $ Assignment i e

  printStatement :: Parser Statement
  printStatement = do e <- string "print" >> char '(' >> expr
                      _ <- char ')'
                      return $ PrintStatement [e]

  compoundStatement :: Parser Statement
  compoundStatement = do s1 <- simpleStatement
                         _ <- spaces >> char ';'
                         spaces
                         s2 <-simpleStatement
                         return $ CompoundStatement s1 s2

  simpleStatement :: Parser Statement
  simpleStatement = try assignment <|> printStatement

  statement :: Parser Statement
  statement = do s <- spaces >> (try compoundStatement <|> simpleStatement)
                 spaces
                 return s

  identExpr :: Parser Expr
  identExpr = do s <- identifier
                 return $ IdentExpr s

  numExpr :: Parser Expr
  numExpr = do ds <- many1 digit
               return $ NumExpr ds

  opExpr :: Parser Expr
  opExpr = do left <- numExpr <|> identExpr
              spaces
              op <- operator
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
  main = case parse (many1 statement)  "example" inputText of
              Left  err -> print err
              Right res -> putStrLn $ "I parsed: '" ++ show res ++ "'"

  inputText :: String
  inputText = "a := 5 + 3; b := (print(a), 10 * a); print(b)"
