module PufCompiler {-(compileAST)-} where

import PufAST
import PufCompilerTypes
import Control.Monad.State

compileAST :: AST String -> String
compileAST ast  = pprint (codegen ast)

pprint :: InstList -> String
pprint (Cat il1 il2) = pprint il1 ++ ('\n' : pprint il2)
pprint i = printInstruction i


codegen ast = evalState (codeV ast) initialState <> Halt

getNextLabel = do (CompilerState e s l) <- get
                  put (CompilerState e s (l+1))
                  return l

codeB :: AST String -> State CompilerState InstList
codeV :: AST String -> State CompilerState InstList 
codeC :: AST String -> State CompilerState InstList 

codeB (Num i) = return (Loadc i)

codeB (App (Prim op) e) = do exp <- codeB e 
                             return (exp <> builtinInst op)

codeB (App (App (Prim op) e1) e2) = do exp1 <- codeB e1
                                       modify (modSD 1)
                                       exp2 <- codeB e2
                                       return (exp1 <> exp2)

codeB (Cond cond e1 e2) = do cxp <- codeB cond
                             la  <- getNextLabel
                             ex1 <- codeB e1
                             lb  <- getNextLabel
                             ex2 <- codeB e2
                             return (cxp 
                                 <> Jumpz la
                                 <> ex1
                                 <> Jump lb
                                 <> Label la
                                 <> ex2
                                 <> Label lb)



codeV (Num i) = return (Loadc i <> Mkbasic)
codeV ex@(App (Prim _) _) = do exp <- codeB ex 
                               return (exp <> Mkbasic)

codeV ex@(App (App (Prim _) _) _) = do exp <- codeB ex
                                       return (exp <> Mkbasic)

codeV (Cond cond e1 e2) = do cxp <- codeB cond
                             la  <- getNextLabel
                             ex1 <- codeV e1
                             lb  <- getNextLabel
                             ex2 <- codeV e2
                             return (cxp 
                                 <> Jumpz la
                                 <> ex1
                                 <> Jump lb
                                 <> Label la
                                 <> ex2
                                 <> Label lb)

--codeV (Rec rdecls asd) = 

codeC = undefined
