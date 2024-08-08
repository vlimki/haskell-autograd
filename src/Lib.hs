module Lib
    ( Value(..)
    , mul
    , add
    , feedForward
    , calculate
    , grad
    ) where

-- Value is essentially just a convenient API to the engine. Most of the calculations are done with the `Calculated` type instead.
data Value
  = Value Double String
  | Add Value Value String
  | Mul Value Value String
  deriving (Show, Eq)

data Calculated
  = Leaf Double String
  | MulBranch Calculated Calculated Double String
  | AddBranch Calculated Calculated Double String
  deriving (Show, Eq)

mul :: Value -> Value -> String -> Value
mul = Mul

add :: Value -> Value -> String -> Value
add = Add

-- If the identifier of the term we're looking for doesn't exist in some branch,
-- we set the gradient of the value to zero, so that it has no effect in the calculations in `grad`.
idMultiplier :: Calculated -> String -> Double
idMultiplier v i = if extractId v == i then 1 else 0

feedForward :: Value -> Calculated
feedForward v = case v of
  Value x idv -> Leaf x idv
  Add (Value x idx) (Value y idy) i -> AddBranch (feedForward $ Value x idx) (feedForward $ Value y idy) (x + y) i
  Mul (Value x idx) (Value y idy) i -> MulBranch (feedForward $ Value x idx) (feedForward $ Value y idy) (x * y) i
  Add x y i -> AddBranch (feedForward x) (feedForward y) (calculate x + calculate y) i
  Mul x y i -> MulBranch (feedForward x) (feedForward y) (calculate x * calculate y) i

grad :: String -> Calculated -> Double
grad id' v = case v of
  AddBranch x y _ i -> if i == id' then 1 else grad id' x + grad id' y
  MulBranch x y _ i -> if i == id' then 1 else (extractCalculation y * grad id' x) + (extractCalculation x * grad id' y)
  x@(Leaf _ _) -> idMultiplier x id'

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

extractId :: Calculated -> String
extractId (Leaf _ id') = id'
extractId (AddBranch _ _ _ id') = id'
extractId (MulBranch _ _ _ id') = id'

extractCalculation :: Calculated -> Double
extractCalculation (Leaf x _) = x
extractCalculation (AddBranch _ _ x _) = x
extractCalculation (MulBranch _ _ x _) = x

