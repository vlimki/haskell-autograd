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

data Outer = Outer {
  _inner :: [Calculated],
  _calculation :: Double,
  _id :: String,
  _grad :: Double
} deriving (Show, Eq)
-- Value is essentially just a convenient API to the engine. The calculations are done with the `Calculated` type instead.
data Value
  = Value Double String
  | Add Value Value String
  | Mul Value Value String
  | ReLU Value String
  | Sigmoid Value String
  deriving (Show, Eq)

data Calculated
  = Leaf Outer 
  | IReLU Outer
  | ISigmoid Outer
  | IMul Outer
  | IAdd Outer
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
  Value x idv -> Leaf (Outer {_inner=[], _calculation=x, _id=idv, _grad=0})
  Add (Value x idx) (Value y idy) i -> IAdd (Outer [feedForward $ Value x idx, feedForward $ Value y idy] (x + y) i 0)
  Mul (Value x idx) (Value y idy) i -> IMul (Outer [feedForward $ Value x idx, feedForward $ Value y idy] (x * y) i 0)
  ReLU (Value x idx) i -> IReLU (Outer [feedForward $ Value x idx] (max x 0) i 0)
  Sigmoid (Value x idx) i -> ISigmoid (Outer [feedForward $ Value x idx] (sigmoid x) i 0)
  ReLU x i -> IReLU (Outer [feedForward x] (max (calculate x) 0) i 0)
  Sigmoid x i -> ISigmoid (Outer [feedForward x] (sigmoid $ calculate x) i 0)
  Add x y i -> IAdd (Outer [feedForward x, feedForward y] (calculate x + calculate y) i 0)
  Mul x y i -> IMul (Outer [feedForward x, feedForward y] (calculate x * calculate y) i 0)

grad :: String -> Calculated -> Double
grad id' v = case v of
  IReLU (Outer x _ i _) -> if i == id' then 1 else (if extractCalculation (head x) > 0 then 1 else 0) * grad id' (head x)
  ISigmoid (Outer x _ i _) -> if i == id' then 1 else sigmoid' $ grad id' (head x)
  IAdd (Outer items _ i _) -> if i == id' then 1 else grad id' (head items) + grad id' (head $ tail items)
  IMul (Outer items _ i _) -> if i == id' then 1 else (extractCalculation (head $ tail items) * grad id' (head items)) + (extractCalculation (head items) * grad id' (head $ tail items))
  x@(Leaf _) -> idMultiplier x id'

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
extractId (Leaf (Outer _ _ id' _)) = id'
extractId (IReLU (Outer _ _ id' _)) = id'
extractId (ISigmoid (Outer _ _ id' _)) = id'
extractId (IAdd (Outer _ _ id' _)) = id'
extractId (IMul (Outer _ _ id' _)) = id'

extractCalculation :: Calculated -> Double
extractCalculation (Leaf (Outer _ calc _ _)) = calc
extractCalculation (IReLU (Outer _ calc _ _)) = calc
extractCalculation (ISigmoid (Outer _ calc _ _)) = calc
extractCalculation (IAdd (Outer _ calc _ _)) = calc
extractCalculation (IMul (Outer _ calc _ _)) = calc
