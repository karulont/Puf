module PufCompilerTypes where

import PufAST
import qualified Data.Map as Map
import qualified Data.Set as Set

data Instruction
     = Mkbasic
     | Getbasic
     | Eval
     | Add
     | Sub
     | Mul
     | Div
     | Leq
     | Eq
     | Apply
     | Update
     | Halt
     | Neg
     | Not
     | Mod
     | Neq
     | Le
     | Geq
     | Gr
     | Or
     | And
     | Nil
     | Cons
     | Loadc Int
     | Pushloc Int
     | Pushglob Int
     | Targ Int
     | Return Int
     | Slide Int
     | Alloc Int
     | Rewrite Int
     | Mkvec Int
     | Getvec Int
     | Get Int
     | Mkclos Int
     | Mkfunval Int
     | Mark Int
     | Jumpz Int
     | Jump Int
     | Tlist Int
     | Move Int Int
     | Label Int
     | Cat Instruction Instruction
     deriving (Show)

data VariableType = Local
                  | Global
                  deriving (Show)

type InstList = Instruction
type Environment = Map.Map String (VariableType,Int)

initialEnvironment = Map.empty :: Environment

envSingle :: String -> (VariableType,Int) -> Environment
envSingle k a = Map.singleton k a

envAdd :: Environment -> (String,(VariableType,Int)) -> Environment
envAdd e (name,v) = Map.insert name v e

envGet :: String -> Environment -> (VariableType,Int)
envGet name e = case Map.lookup name e of
                  Just r -> r
                  Nothing -> error ("Unknown variable " ++ name)
                  
envUnion :: Environment -> Environment -> Environment
{- Map.union prefers elements from the left map, 
 - but when using envUnion we want to prefer 
 - elements from right map because that is more local scope.
 -} 
envUnion m1 m2 = Map.unionWith (\_ b -> b) m1 m2

data CompilerState = CompilerState [(Char,Int)] Int Int
                     deriving (Show)

initialState = CompilerState [] 0 0

modSD :: Int -> CompilerState -> CompilerState
modSD x (CompilerState e sd l) = CompilerState e (sd+x) l

infixr 6 <>

(<>) :: InstList -> InstList -> InstList
x <> y = Cat x y

free :: AST String -> [String]
free ast = Set.elems $ free' Set.empty ast
type Variables = Set.Set String

free' :: Variables -> AST String -> Variables
free' bound (Var name) = if Set.member name bound then Set.empty 
                                                  else Set.singleton name
free' _ (Num _) = Set.empty
free' _ (Prim _) = Set.empty
free' b (Cond c e0 e1) = free' b c `Set.union` free' b e0 
                                   `Set.union` free' b e1
free' b (Fun name e) = free' (Set.insert name b) e
free' b (App e0 e1) = free' b e0 `Set.union` free' b e1
free' bound (Let decls e)
    = let bounds = scanl Set.union bound (
                     map (\l -> case l of
                                  Sdecl n _ -> Set.singleton n
                                  Tdecl ns _ -> foldl (\s e -> Set.insert e s) 
                                                    Set.empty ns
                         ) decls
                    )
      in foldl1 Set.union 
           (zipWith (\l b -> case l of
                               Sdecl _ e -> free' b e
                               Tdecl _ e -> free' b e
                    ) decls bounds)

free' b (Tuple exprs) = foldl (\s e -> Set.union s (free' b e)) Set.empty exprs
free' b (Select _ e) = free' b e
free' _ (PufAST.Nil) = Set.empty
free' b (PufAST.Cons e0 e1) = free' b e0 `Set.union` free' b e1
free' b (Case e0 e1 h t e2) = free' b e0 `Set.union` free' b e1 `Set.union`
                                free' (foldr Set.insert b [h,t])  e2
      

printInstruction :: Instruction -> String
printInstruction x = case x of
                       Mkbasic -> "mkbasic"
                       Getbasic -> "getbasic"
                       Eval -> "eval"
                       Add -> "add"
                       Sub -> "sub"
                       Mul -> "mul"
                       Div -> "div"
                       Leq -> "leq"
                       Eq -> "eq"
                       Apply -> "apply"
                       Update -> "update"
                       Halt -> "halt"
                       Neg -> "neg"
                       Not -> "not"
                       Mod -> "mod"
                       Neq -> "neq"
                       Le -> "le"
                       Geq -> "geq"
                       Gr -> "gr"
                       Or -> "or"
                       And -> "and"
                       PufCompilerTypes.Nil -> "nil"
                       PufCompilerTypes.Cons -> "cons"
                       Loadc i -> "loadc " ++ show i
                       Pushloc i -> "pushloc " ++ show i
                       Pushglob i -> "pushglob " ++ show i
                       Targ i -> "targ " ++ show i
                       Return i -> "return " ++ show i
                       Slide i -> "slide " ++ show i
                       Alloc i -> "alloc " ++ show i
                       Rewrite i -> "rewrite " ++ show i
                       Mkvec i -> "mkvec " ++ show i
                       Getvec i -> "getvec " ++ show i
                       Get i -> "get " ++ show i
                       Mkclos i -> "mkclos _" ++ show i
                       Mkfunval i -> "mkfunval _" ++ show i
                       Mark i -> "mark _" ++ show i
                       Jumpz i -> "jumpz _" ++ show i
                       Jump i -> "jump _" ++ show i
                       Tlist i -> "tlist _" ++ show i
                       Move i1 i2 -> "move " ++ show i1 ++ " " ++ show i2
                       Label i -> "\t_" ++ show i ++ ":"
                       _ -> error "pInst"

builtinInst :: Builtin -> Instruction
builtinInst UNeg = Neg
builtinInst UNot = Not
builtinInst BAdd = Add
builtinInst BSub = Sub
builtinInst BMul = Mul
builtinInst BDiv = Div
builtinInst BMod = Mod
builtinInst BEq = Eq
builtinInst BNe = Neq
builtinInst BLe = Leq
builtinInst BLt = Le
builtinInst BGe = Geq
builtinInst BGt = Gr
builtinInst BOr = Or
builtinInst BAnd = And
