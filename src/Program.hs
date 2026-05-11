-- Pilar Frutos and Santino Pepe
module Program(T, parse, fromString, toString, exec) where

import Parser hiding (T)
import qualified Statement
import qualified Dictionary
import Prelude hiding (return, fail)

newtype T = Program [Statement.T]

instance Eq T where
  (Program s1) == (Program s2) = concatMap Statement.toString s1 == concatMap Statement.toString s2

instance Show T where
  show = toString

instance Parse T where
  parse cs =
    case iter Statement.parse cs of
    Just (ss, cs') -> Just (Program ss, cs')
    Nothing        -> Nothing
  toString (Program ss) = concatMap Statement.toString ss

exec :: T -> [Integer] -> [Integer]
exec (Program ss) = Statement.execute ss Dictionary.empty 
