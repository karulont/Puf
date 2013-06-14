module PufAST (AST(..), Builtin(..), LDecl(..), RDecl(..), ppAST) where

import Text.PrettyPrint.Leijen


-- Abstract syntax tree parametrized over the type of variables
data AST a
    = Var a                         -- variables (normally Srings)
    | Num Int                       -- numeric constants
    | Prim Builtin                  -- primitive operators
    | Cond (AST a) (AST a) (AST a)  -- if-then-else
    | Fun a (AST a)                 -- lambda-abstraction
    | App (AST a) (AST a)           -- application
    | Let [LDecl a] (AST a)         -- let
    | Rec [RDecl a] (AST a)         -- letrec
    -- tuples
    | Tuple [AST a]
    | Select Int (AST a)
    -- lists
    | Nil
    | Cons (AST a) (AST a)
    | Case (AST a) (AST a) a a (AST a)
    deriving Show


data Builtin                -- primitive operators
    = UNeg
    | UNot
    | BAdd
    | BSub
    | BMul
    | BDiv
    | BMod
    | BEq
    | BNe
    | BLe
    | BLt
    | BGe
    | BGt
    | BOr
    | BAnd
    deriving Show

data LDecl a
    = Sdecl a (AST a)      -- simple declaration
    | Tdecl [a] (AST a)    -- tuple declaration
    deriving Show

data RDecl a
    = Rdecl a (AST a)      -- letrec declaration
    deriving Show



-- pretty printing
ppAST  :: Show a => AST a -> String
ppAST e = show $ pprogram e

texta :: Show a=> a -> Doc
texta a = text $ filter (\c -> c /= '"' ) $ show a

pprogram (Rec decls a) = vsep $ map prdecl decls
pprogram _ = error "Program not in normal form!"

pprint (Var a)  = texta a
pprint (Num a)  = int a
pprint (Prim b) = pprim b
pprint (Cond cond i e) = text "if" <+> pprint cond </> text "then" 
                           <$> indent 2 (pprint i) <$> text "else"
                           <$> indent 2 (pprint e)

pprint (Fun a b) = text "fn" <+> texta a <+> text "->" <+> pprint b

pprint (App (App (Prim UNeg) a) b@(App _ _)) =  pprim UNeg <+> pprint a 
                                                <+> parens (pprint b)
pprint (App (App (Prim UNot) a) b@(App _ _)) =  pprim UNot <+> pprint a 
                                                <+> parens (pprint b)
pprint (App (App (Prim p) a) b@(App _ _)) =  pprint a <+> pprim p 
                                             <+> parens (pprint b)
pprint (App (App (Prim UNeg) a) b) =  pprim UNeg <+> pprint a <+> pprint b
pprint (App (App (Prim UNot) a) b) =  pprim UNot <+> pprint a <+> pprint b
pprint (App (App (Prim p) a) b) =  pprint a <+> pprim p <+> pprint b

pprint (App a inner@(App b c)) = pprint a <+> parens (pprint inner)
pprint (App a b) = pprint a <+> pprint b
pprint (Let decls a) = align (text "let" </> align (vsep (map pldecl decls))
                            </> (text "in" <+> pprint a))
pprint (Rec decls a) = align (text "letrec" </> align (vsep (map prdecl decls))
                         </> (text "in" <+> pprint a))
pprint (Tuple a) = align $ parens $ fillSep $ punctuate comma (map pprint a)
pprint (Select i a) = text "#" <> int i <+> pprint a
pprint Nil = text "[]"
pprint cons@(Cons a b)
    | endsWithNil b = align $ lbracket <> pprint a <> ppl b
    | otherwise = parens $ ppc cons
    where ppl Nil = rbracket
          ppl (Cons a b) = comma </> pprint a <//> ppl b
          ppc (Cons a b) = pprint a <> colon <> ppc b
          ppc e = pprint e
          endsWithNil Nil = True
          endsWithNil (Cons _ b) = endsWithNil b
          endsWithNil _ = False


pprint (Case cond empt x y part) = hang 2 (text "case" <+> pprint cond <+> text "of"
                                     <$> fill l (text "[]") <+> text "->"
                                          <+> pprint empt <> semi <$> fill l calt
                                          <+> text "->" <+> pprint part)
                                          where l = length (show calt)
                                                calt = texta x <> colon <> texta y


prdecl :: Show a => RDecl a -> Doc
prdecl (Rdecl a b) = ppdecl a b

pldecl :: Show a => LDecl a -> Doc
pldecl (Sdecl a b) = ppdecl a b
pldecl (Tdecl a b) = parens (fillSep (punctuate comma (map texta a)))
                     <+> equals <+> pprint b <> semi

ppdecl a b@(Fun _ _) = texta a <+> ppfun b
                       where ppfun (Fun f a) = texta f <+> ppfun a
                             ppfun x = equals <+> pprint x <> semi
ppdecl a b = texta a <+> equals <+> pprint b <> semi

pprim b = text (case b of
           UNeg  -> "neg" 
           UNot  -> "not"
           BAdd  -> "+"
           BSub  -> "-"
           BMul  -> "*"
           BDiv  -> "/"
           BMod  -> "%"
           BEq   -> "=="
           BNe   -> "/="
           BLe   -> "<="
           BLt   -> "<"
           BGe   -> ">="
           BGt   -> ">"
           BOr   -> "||"
           BAnd  -> "&&")
