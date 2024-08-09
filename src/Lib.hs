module Lib
  ( Value (..)
  , mul
  , add
  , feedForward
  , calculate
  , grad
  , (!*)
  , (!+)
  ) where

-- Value is essentially just a convenient API to the engine. The calculations are done with the `Calculated` type instead.
data Value
  = Value Double String
  | Add Value Value String
  | Mul Value Value String
  | ReLU Value String
  | Sigmoid Value String
  deriving (Show, Eq)

data Calculated
  = Leaf Double String
  | IReLU Calculated Double String
  | ISigmoid Calculated Double String
  | IMul Calculated Calculated Double String
  | IAdd Calculated Calculated Double String
  deriving (Show, Eq)

mul :: Value -> Value -> String -> Value
mul = Mul

add :: Value -> Value -> String -> Value
add = Add

(!+) :: Value -> Value -> String -> Value
(!+) = add

(!*) :: Value -> Value -> String -> Value
(!*) = mul

sigmoid :: Double -> Double
sigmoid x = 1 / (1 + exp (-x))

sigmoid' :: Double -> Double
sigmoid' x = sigmoid x * (1 - sigmoid x)

-- If the identifier of the term we're looking for doesn't exist in some branch,
-- we set the gradient of the value to zero, so that it has no effect in the calculations in `grad`.
idMultiplier :: Calculated -> String -> Double
idMultiplier v i = if extractId v == i then 1 else 0

feedForward :: Value -> Calculated
feedForward v = case v of
  Value x idv -> Leaf x idv
  Add (Value x idx) (Value y idy) i -> IAdd (feedForward $ Value x idx) (feedForward $ Value y idy) (x + y) i
  Mul (Value x idx) (Value y idy) i -> IMul (feedForward $ Value x idx) (feedForward $ Value y idy) (x * y) i
  ReLU (Value x idx) i -> IReLU (feedForward $ Value x idx) (max x 0) i
  Sigmoid (Value x idx) i -> ISigmoid (feedForward $ Value x idx) (sigmoid x) i
  ReLU x i -> IReLU (feedForward x) (max (calculate x) 0) i
  Sigmoid x i -> ISigmoid (feedForward x) (sigmoid (calculate x)) i
  Add x y i -> IAdd (feedForward x) (feedForward y) (calculate x + calculate y) i
  Mul x y i -> IMul (feedForward x) (feedForward y) (calculate x * calculate y) i

grad :: String -> Calculated -> Double
grad id' v = case v of
  IReLU x _ i -> if i == id' then (if extractCalculation x > 0 then 1 else 0) else grad id' x
  ISigmoid x _ i -> if i == id' then sigmoid' $ extractCalculation x else grad id' x
  IAdd x y _ i -> if i == id' then 1 else grad id' x + grad id' y
  IMul x y _ i -> if i == id' then 1 else (extractCalculation y * grad id' x) + (extractCalculation x * grad id' y)
  x@(Leaf _ _) -> idMultiplier x id'

calculate :: Value -> Double
calculate v = case v of
  Value x _ -> x
  Add (Value x _) (Value y _) _ -> x + y
  Mul (Value x _) (Value y _) _ -> x * y
  Mul x y _ -> calculate x * calculate y
  Add x y _ -> calculate x + calculate y
  ReLU x _ -> max (calculate x) 0
  Sigmoid x _ -> sigmoid (calculate x)

-- backProp :: Calculated -> Calculated
-- backProp v = case v of
-- AddBranch x y

extractId :: Calculated -> String
extractId (Leaf _ id') = id'
extractId (IReLU _ _ id') = id'
extractId (IAdd _ _ _ id') = id'
extractId (IMul _ _ _ id') = id'

extractCalculation :: Calculated -> Double
extractCalculation (Leaf x _) = x
extractCalculation (IReLU _ x _) = x
extractCalculation (IAdd _ _ x _) = x
extractCalculation (IMul _ _ x _) = x
