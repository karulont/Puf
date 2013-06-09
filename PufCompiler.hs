module PufCompiler {-(compileAST)-} where

import PufAST
import PufCompilerTypes
import Control.Monad.State

compileAST :: AST String -> String
compileAST ast  = pprint (codegen ast)

pprint :: InstList -> String
pprint (Cat il1 il2) = pprint il1 ++ ('\n' : pprint il2)
pprint i = printInstruction i


codegen ast = evalState (codeV ast initialEnvironment 0) initialState <> Halt

getNextLabel = do (CompilerState e s l) <- get
                  put (CompilerState e s (l+1))
                  return l

getvar :: String -> Environment -> Int -> Instruction
getvar name e sd = case envGet name e of
                     (Local,i) -> Pushloc (sd-i)
                     (Global,i) -> Pushglob i

codeB :: AST String -> Environment -> Int -> State CompilerState InstList
codeV :: AST String -> Environment -> Int -> State CompilerState InstList
codeC :: AST String -> Environment -> Int -> State CompilerState InstList

codeB (Num i) _ _ = return (Loadc i)

codeB (App (Prim op) e) p sd
    = do exp <- codeB e p sd
         return (exp <> builtinInst op)

codeB (App (App (Prim op) e1) e2) p sd
    = do exp1 <- codeB e1 p sd
         exp2 <- codeB e2 p (sd+1)
         return (exp1 <> exp2 <> builtinInst op)

codeB (Cond cond e1 e2) p sd
    = do cxp <- codeB cond p sd
         la  <- getNextLabel
         ex1 <- codeB e1 p sd
         lb  <- getNextLabel
         ex2 <- codeB e2 p sd
         return (cxp <> Jumpz la <> ex1 <> Jump lb <> Label la 
                     <> ex2 <> Label lb)

codeB e p sd = do exp <- codeV e p sd
                  return $ exp <> Getbasic

codeV (Num i) _ _ = return (Loadc i <> Mkbasic)
codeV (Var a) e sd = return (getvar a e sd <> Eval)
codeV ex@(App (Prim _) _) p sd
    = do exp <- codeB ex p sd
         return (exp <> Mkbasic)

codeV ex@(App (App (Prim _) _) _) p sd
    = do exp <- codeB ex p sd
         return (exp <> Mkbasic)

codeV (Cond cond e1 e2) p sd
    = do cxp <- codeB cond p sd
         la  <- getNextLabel
         ex1 <- codeV e1 p sd
         lb  <- getNextLabel
         ex2 <- codeV e2 p sd
         return (cxp <> Jumpz la <> ex1 <> Jump lb <> Label la
                     <> ex2 <> Label lb)

codeV fn@(Fun _ _) p sd
    = do let z = free(fn)
             (x,e) = getLambdaSplit fn
             g = length z
             k = length x
             vars = zipWith (\x i -> getvar x p (sd+i)) z [0..(g-1)]
             p' = (foldl envAdd initialEnvironment
                        (zipWith (\x i -> (x,(Local,-i))) x [0..(k-1)]))
                  `envUnion`
                  (foldl envAdd initialEnvironment
                        (zipWith (\z j -> (z,(Global,j))) z [0..(g-1)]))
         la <- getNextLabel
         exp <- codeV e p' 0
         lb <- getNextLabel
         return ( (if null vars then Mkvec g else foldl1 Cat vars <> Mkvec g)
                    <> Mkfunval la <> Jump lb <> Label la <> Targ k <> exp
                    <> Return k <> Label lb)
           where getLambdaSplit (Fun n e) = let (a,b) = getLambdaSplit e
                                           in (n:a,b)
                 getLambdaSplit e = ([],e)

codeV ap@(App _ _) p sd
    = do let (e':es) = apps ap
             e = reverse es
             m = length e
         la <- getNextLabel
         exps <- zipWithM (\e i -> codeC e p (sd + i)) e [3..(m+2)]
         ex <- codeV e' p (sd + m + 3)
         return $ Mark la <> foldl1 Cat exps <> ex <> Apply <> Label la
           where apps (App ap@(App _ _) e) = apps ap++[e]
                 apps (App e1 e2) = e1:e2:[]
                 apps _ = []

codeV (Let [Tdecl ys e1] e0) p sd
    = do let k = length ys
             p' = foldl envAdd p (
                            zipWith (\y i -> (y,(Local,sd+i))) ys 
                            [0..(k-1)])
         ex1 <- codeV e1 p sd
         ex0 <- codeV e0 p' (sd+k-1)
         return $ ex1 <> Getvec k <> ex0 <> Slide k


codeV (Let ldecls e0) p sd
    = do let n = length ldecls
             ps = scanl envUnion p (zipWith
                    (\l j -> case l of 
                               Sdecl n _ -> envSingle n (Local,sd+j) 
                               Tdecl ns _ -> undefined
                    ) ldecls [1..(n)])
         exps <- zipWithM 
                 (\d (env,i) -> case d of
                             Sdecl _ e -> codeC e env (sd+i)
                             Tdecl _ e -> codeC e env (sd+i)
                 ) ldecls (zip ps [0..(n-1)])
         ex <- codeV e0 (last ps) (sd+n)
         return $ foldl1 Cat exps <> ex <> Slide n
        

codeV (Rec rdecls e0) p sd
    = do let n = length rdecls
             p' = foldl envAdd p (map (\((Rdecl name _),i) -> 
                                      (name,(Local,sd+i))) (zip rdecls [1..n]))
         exps <- forM rdecls (\(Rdecl name ei) -> 
            codeC ei p' (sd+n))
         exp0 <- codeV e0 p' (sd+n)
         return $ 
            foldl Cat (Alloc n) 
                (zipWith (\e i -> e <> Rewrite i) exps [n,(n-1)..1])
            <> exp0 <> Slide n

codeV (Tuple es) p sd
    = do let k = length es
             exprs = zipWithM (\e i -> codeC e p (sd+i)) es [0..k]
         exs <- exprs
         return $ foldl1 Cat exs <> Mkvec k

codeV (Select j e) p sd
    = do ex <- codeV e p sd
         return $ ex <> Get j <> Eval

codeC e p sd = do let z = free(e)
                      g = length z
                      iRange = [0..(g-1)]
                      vars = map (\(x,i) -> getvar x p (sd+i)) (zip z iRange)
                      p' = foldl envAdd initialEnvironment (
                                zipWith (\n i -> (n,(Global,i))) z iRange )
                  la <- getNextLabel
                  exp <- codeV e p' 0
                  lb <- getNextLabel
                  return ((if null vars then Mkvec g 
                                    else foldl1 Cat vars <> Mkvec g)
                                      <> Mkclos la <> Jump lb <> Label la
                                      <> exp <> Update <> Label lb)


