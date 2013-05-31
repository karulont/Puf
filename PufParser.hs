module PufParser (parsePuf) where

import PufAST
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as Tok
import Text.ParserCombinators.Parsec.Expr
import Control.Monad

-- parser of puf-programs
parsePuf :: String -> AST String
parsePuf input = case parse compilationUnit "puffail" input of
   Left err -> error (show err) :: AST String
   Right val -> val

compilationUnit :: Parser (AST String)
compilationUnit = do { whiteSpace
                     ; p <- program
                     ; eof
                     ; return p
                     }

-- A program is eqiuvalent of a letrec-expression:
-- 'letrec' <rdecl> ... <rdecl> 'in' 'main';
program :: Parser (AST String)
program = do { 
             ; rdecls <- many1 rdecl
             ; return (Rec rdecls (Var "main"))
             }

rdecl :: Parser (RDecl String)
rdecl = do { (flhs:rest) <- many1 identifier
           ; reservedOp "="
           ; e <- expr
           ; semi
           ; return (Rdecl flhs (foldr Fun e rest))
           }
        <?> "rdecl"

ldecl :: Parser (LDecl String)
ldecl = do { (flhs:rest) <- many1 identifier
           ; reservedOp "="
           ; e <- expr
           ; semi
           ; return (Sdecl flhs (foldr Fun e rest))
           }
    <|> do { tlhs <- parens (commaSep identifier)
           ; reservedOp "="
           ; e <- expr
           ; semi
           ; return (Tdecl tlhs e)
           }
    <?> "ldecl"

expr :: Parser (AST String)
expr = choice [ binaryOperExpr
              , simpleExpr
              ]
        <?> "expression"


varExpr :: Parser (AST String)
varExpr =  do { ident <- identifier
	      ; return (Var ident)
	      }
            <?> "variable expression"

numExpr :: Parser (AST String)
numExpr = do { num <- lexeme decimal
             ; return (Num $ fromInteger num)
             }
            <?> "number expression"

tupleExpr :: Parser (AST String)
tupleExpr = do { tuples <- parens $ commaSep expr
               ; return (case tuples of
                           [first] -> first
                           _       -> Tuple tuples)
               }
            <?> "tuple expression"

listExpr :: Parser (AST String)
listExpr = do { members <- squares $ commaSep expr
              ; return (foldr Cons Nil members)
              }
            <?> "list expression"

unaryOperExpr :: Parser (AST String)
unaryOperExpr = do { oper <- uop
                   ; e    <- expr
                   ; return (App oper e)
                   }
            <|> do { reservedOp "#"
                   ; num <- lexeme decimal
                   ; e <- expr
                   ; return (Select (fromInteger num) e)
                   }
            <?> "unaryOper expression"

ifThenElseExpr :: Parser (AST String)
ifThenElseExpr = do { reserved "if"
                    ; cond <- expr
                    ; reserved "then"
                    ; e1   <- expr
                    ; reserved "else"
                    ; e2   <- expr
                    ; return (Cond cond e1 e2)
                    }
                <?> "if-then-else expression"

fnExpr :: Parser (AST String)
fnExpr = do { reserved "fn"
            ; ident <- many identifier
            ; reservedOp "->"
            ; e <- expr
            ; return (foldr Fun e ident)
            }
          <?> "lambda expression"

caseExpr :: Parser (AST String)
caseExpr =  do { reserved "case"
               ; cond <- expr
               ; reserved "of"
               ; reservedOp "[]"
               ; reservedOp "->"
               ; nalt <- expr
               ; semi
               ; x <- identifier
               ; colon
               ; y <- identifier
               ; reservedOp "->"
               ; calt <- expr
               ; return (Case cond nalt x y calt)
               }
          <?> "case expression"

letExpr :: Parser (AST String)
letExpr = do { reserved "let"
             ; ldecls <- many1 ldecl
             ; reserved "in"
             ; e <- expr
             ; return (Let ldecls e)
             }
          <?> "let expression"

letrecExpr :: Parser (AST String)
letrecExpr = do { reserved "letrec"
                ; rdecls <- many1 rdecl
                ; reserved "in"
                ; e <- expr
                ; return (Rec rdecls e)
                }
          <?> "letrec expression"

binaryOperExpr :: Parser (AST String)
binaryOperExpr = buildExpressionParser operators applyExpr <?> "binaryOper expression"

applyExpr :: Parser (AST String)
applyExpr = do { exprs <- many1 simpleExpr
               ; return (foldl1 App exprs)
               }
          <?> "apply expression"

simpleExpr :: Parser (AST String)
simpleExpr = choice [ try (parens expr)
                    , tupleExpr
                    , listExpr
                    , unaryOperExpr
                    , ifThenElseExpr 
                    , caseExpr
                    , fnExpr
                    , letExpr
                    , letrecExpr
                    , numExpr
                    , varExpr
                    ]
             <?> "simple expression"

uop :: Parser (AST String)
uop = do { reserved "neg"
         ; return (Prim UNeg)
         }
 <|>  do { reserved "not"
         ; return (Prim UNot)
         }
 <?>  "uop"

operators = [ [binary "*" BMul, binary "/" BDiv, binary "%" BMod]
            , [binary "+" BAdd, binary "-" BSub]
            , [Infix (do { reservedOp ":"; return Cons }) AssocRight]
            , [binary "<" BLt, binary "<=" BLe, binary ">" BGt, binary ">=" BGe]
            , [binary "==" BEq, binary "/=" BNe]
            , [binary "&&" BAnd]
            , [binary "||" BOr]
            ]
        where
          binary name fun = Infix (
                do{ reservedOp name
                  ; return (\a b-> App (App (Prim fun) a) b)}
                  ) AssocLeft

-- Languagedef
pufDef :: Tok.LanguageDef st
pufDef = Tok.LanguageDef { 
             Tok.commentStart = "/*",
             Tok.commentEnd = "*/",
             Tok.commentLine = "//",
             Tok.nestedComments = True,
             Tok.identStart = letter,
             Tok.identLetter = alphaNum,
             Tok.opStart = oneOf (concat (Tok.reservedOpNames pufDef)),
             Tok.opLetter = oneOf "=&|",
             Tok.reservedNames = ["if", "then", "else", "fn", "let", "letrec", "in",
                                    "case", "of", "neg", "not"],
             Tok.reservedOpNames = ["*", "/", "%", "+", "-", ":", "<", "<=", ">",
                                    ">=", "==", "/=", "&&", "||", "=", "->", "#",
                                    "[]" ],
             Tok.caseSensitive = True}

puf = Tok.makeTokenParser pufDef

whiteSpace = Tok.whiteSpace puf
identifier = Tok.identifier puf
reserved = Tok.reserved puf
operator = Tok.operator puf
decimal = Tok.decimal puf
commaSep = Tok.commaSep puf
commaSep1 = Tok.commaSep1 puf
parens = Tok.parens puf
squares = Tok.squares puf
lexeme = Tok.lexeme puf
symbol = Tok.symbol puf
reservedOp = Tok.reservedOp puf
semi = Tok.semi puf
colon = Tok.colon puf
