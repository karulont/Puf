module PufCompilerTypes where

import PufAST

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

type InstList = Instruction


data CompilerState = CompilerState [(Char,Int)] Int Int
                     deriving (Show)

initialState = CompilerState [] 0 0

modSD :: Int -> CompilerState -> CompilerState
modSD x (CompilerState e sd l) = CompilerState e (sd+x) l

infixr 6 <>

(<>) :: InstList -> InstList -> InstList
x <> y = Cat x y
                                    
printInstruction :: Instruction -> String
printInstruction x = case x of
                       Mkbasic -> "mkbasic"
                       Getbasic -> "getbasic"
                       Label i -> "\t_" ++ show i
                       _ -> ""

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
