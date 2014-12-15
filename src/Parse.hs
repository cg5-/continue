module Parse where
import ParseAST
import EvalAST (InfixOperator, infixOperatorTable, PrefixOperator, prefixOperatorTable)
import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Token (GenLanguageDef(..))
import Text.Parsec.Expr
import Data.Map.Strict (toList, elems)
import Data.List (groupBy, sortBy)
import Data.Function (on)

type Parser = Parsec String ()

----------

lexer :: P.TokenParser ()
lexer = P.makeTokenParser $ LanguageDef {
  commentStart = "/*",
  commentEnd = "*/",
  commentLine = "//",
  nestedComments = False,
  identStart = letter,
  identLetter = alphaNum <|> char '_',
  opStart = oneOf "+-*/<>=&|!%",
  opLetter = oneOf "+-*/<>=&|!%",
  reservedNames = ["true", "false", "as"],
  reservedOpNames = map snd $ elems prefixOperatorTable ++ elems infixOperatorTable,
  caseSensitive = True
}

whiteSpace = P.whiteSpace lexer
identifier = P.identifier lexer
reserved = P.reserved lexer
reservedOp = P.reservedOp lexer
stringLiteral = P.stringLiteral lexer
integer = P.natural lexer
symbol = P.symbol lexer
lexeme = P.lexeme lexer
parens = P.parens lexer
braces = P.braces lexer
angles = P.angles lexer
squares = P.brackets lexer
semi = P.semi lexer
comma = P.comma lexer
colon = P.colon lexer
dot = P.dot lexer
commaSep = P.commaSep lexer
commaSep1 = P.commaSep1 lexer

----------

operatorTable =
  let groupedInfixes = groupBy ((==) `on` (fst.snd)) $ toList infixOperatorTable
      sortedInfixGroups = sortBy (compare `on` (fst.head)) groupedInfixes
      convertInfix (op, (_, symbol)) = Infix (reservedOp symbol >> return (InfixOperation op)) AssocLeft
      infixes = map (map convertInfix) sortedInfixGroups

      groupedPrefixes = groupBy ((==) `on` (fst.snd)) $ toList prefixOperatorTable
      sortedPrefixGroups = sortBy (compare `on` (fst.head)) groupedPrefixes
      convertPrefix (op, (_, symbol)) = Prefix (reservedOp symbol >> return (PrefixOperation op))
      prefixes = map (map convertPrefix) sortedPrefixGroups

  in prefixes ++ infixes

----------

expr :: Parser Expr
expr = try ifElse <|> operations <?> "expression" where
  ifElse = do
    cond <- operations
    symbol "?"
    ifTrue <- operations
    symbol ":"
    ifFalse <- expr
    return $ IfElse cond ifTrue ifFalse

operations :: Parser Expr
operations = buildExpressionParser operatorTable term <?> "expression"

(>:) :: Parser a -> (a -> b) -> Parser b
(>:) = flip fmap

simpleTerm :: Parser Expr
simpleTerm = integer >: IntLiteral
     <|> stringLiteral >: StringLiteral
     <|> identifier >: VarAccess
     <|> (reserved "true" >> (return $ BoolLiteral True))
     <|> (reserved "false" >> (return $ BoolLiteral False))
     <|> squares (commaSep expr) >: foldr (\a b -> TupleLiteral [a, b]) (TupleLiteral [])
     <|> parensExpr
     <|> block >: BlockLiteral
     <?> "simple term"

parensExpr = parens $ (do
    first <- expr
    (do comma
        rest <- commaSep1 expr
        return (TupleLiteral (first : rest))
      ) <|> return first
  ) <|> return (TupleLiteral [])

term :: Parser Expr
term = try yield <|> startingWithSimpleTerm <?> "term" where
  yield = do
    listenerName <- identifier
    symbol "!"
    arg <- parensExpr <|> expr <|> return (TupleLiteral [])
    return $ Yield listenerName arg

  startingWithSimpleTerm = do
    initial <- simpleTerm
    let qualifiedYield = do
          symbol "'"
          listenerName <- identifier
          symbol "!"
          arg <- parensExpr <|> expr <|> return (TupleLiteral [])
          return $ QualifiedYield initial listenerName arg
    let funcCall = do
          arg <- term
          return $ FCall initial arg
    qualifiedYield <|> funcCall <|> return initial


lvalue :: Parser LValue
lvalue =  symbol "_" >: (const NobodyCares)
      <|> identifier >: VarBind
      <|> squares (commaSep lvalue) >: foldr (\a b -> TuplePattern [a, b]) (TuplePattern [])
      <|> parensLValue
      <?> "lvalue"
  where
    parensLValue = parens $ (do
        first <- lvalue
        (do comma
            rest <- commaSep1 lvalue
            return (TuplePattern (first : rest))
          ) <|> return first
      ) <|> return (TuplePattern [])

block :: Parser Block
block = braces $ do
  label <- optionMaybe (symbol "'" >> identifier)
  arg <- try argument <|> return NobodyCares
  stmts <- statements
  return $ Block label arg stmts
  where
    argument = do
      lval <- lvalue
      symbol "->"
      return lval

statements :: Parser [Statement]
statements = many (try bind <|> try advance <|> exprStatement <?> "statement")
  where
    bind = do
      lval <- lvalue
      symbol "="
      value <- expr
      semi
      return $ Bind lval value
    advance = do
      block <- simpleTerm
      arg <- term <|> return (TupleLiteral [])
      symbol ">>"
      listeners <- commaSep1 listener
      semi
      return $ Advance block arg listeners
      where
        listener :: Parser (String, LValue, [Statement])
        listener = do
          listenerName <- identifier
          lval <- (symbol "as" >> lvalue) <|> return NobodyCares
          stmts <- squares statements <|> return []
          return $ (listenerName, lval, stmts)
    exprStatement = do
      value <- expr
      semi
      return $ ExprStatement value

fileParser :: Parser [Statement]
fileParser = do
  whiteSpace
  stmts <- statements
  eof
  return stmts
