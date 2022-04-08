module Types where 

import Data.SExpresso.SExpr 

data Op = Eq | And | Or | Leq | Geq | Le | Ge | Neg | Add | Mult | Assert

data Node = Imm Int | Fn String | B Bool

