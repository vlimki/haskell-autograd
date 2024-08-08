module Lib
    ( Value(..)
    , mul
    , add
    , feedForward
    , calculate
    ) where

data Value = Value Double String
  | Add Value Value String
  | Mul Value Value String
  deriving (Show, Eq)

data Calculated = Leaf Value Double
  | MulBranch Calculated Calculated Double String
  | AddBranch Calculated Calculated Double String
  deriving (Show, Eq)

mul :: Value -> Value -> String -> Value
mul = Mul

add :: Value -> Value -> String -> Value
add = Add

feedForward :: Value -> Calculated
feedForward v = case v of
  Value x idv -> Leaf (Value x idv) x
  Add (Value x idx) (Value y idy) i -> AddBranch (feedForward $ Value x idx) (feedForward $ Value y idy) (x + y) i
  Mul (Value x idx) (Value y idy) i -> MulBranch (feedForward $ Value x idx) (feedForward $ Value y idy) (x * y) i
  Add x y i -> AddBranch (feedForward x) (feedForward y) (calculate x + calculate y) i
  Mul x y i -> MulBranch (feedForward x) (feedForward y) (calculate x * calculate y) i

grad :: Double -> Double
grad = id

calculate :: Value -> Double
calculate v = case v of
  Value x _ -> x
  Add (Value x _) (Value y _) _ -> x + y
  Mul (Value x _) (Value y _) _ -> x * y
  Mul x y _ -> calculate x * calculate y
  Add x y _-> calculate x + calculate y

--backProp :: Calculated -> Calculated
-- backProp v = case v of
-- AddBranch x y 
