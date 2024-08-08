module Lib
    ( Value(..)
    , mul
    , add
    , feedForward
    , calculate
    ) where

data Value = Value Double
  | Add Value Value
  | Mul Value Value
  deriving (Show, Eq)

data Calculated = Leaf Value Double
  | MulBranch Calculated Calculated Double 
  | AddBranch Calculated Calculated Double 
  deriving (Show, Eq)

mul :: Value -> Value -> Value
mul = Mul

add :: Value -> Value -> Value
add = Add

feedForward :: Value -> Calculated
feedForward v = case v of
  Value x -> Leaf (Value x) x
  Add (Value x) (Value y) -> AddBranch (feedForward $ Value x) (feedForward $ Value y) (x + y)
  Mul (Value x) (Value y) -> MulBranch (feedForward $ Value x) (feedForward $ Value y) (x * y)
  Add x y -> AddBranch (feedForward x) (feedForward y) (calculate x + calculate y)
  Mul x y -> MulBranch (feedForward x) (feedForward y) (calculate x * calculate y)

calculate :: Value -> Double
calculate v = case v of
  Value x -> x
  Add (Value x) (Value y) -> x + y
  Mul (Value x) (Value y) -> x * y
  Mul x y -> calculate x * calculate y
  Add x y -> calculate x + calculate y
