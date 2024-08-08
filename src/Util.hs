module Util 
  ( extractId
  , extractCalculation
  ) where

extractId :: Calculated -> String
extractId (Leaf _ id') = id'
extractId (AddBranch _ _ _ id') = id'
extractId (MulBranch _ _ _ id') = id'

extractCalculation :: Calculated -> Double
extractCalculation (Leaf x _) = x
extractCalculation (AddBranch _ _ x _) = x
extractCalculation (MulBranch _ _ x _) = x

