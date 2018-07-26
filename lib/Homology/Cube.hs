{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Homology.Cube where

import           Control.Applicative
import           Control.Monad
import           Data.Char
import           Data.Monoid
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Data.Vector                  (Vector)
import qualified Data.Vector                  as V
import           Text.ParserCombinators.ReadP

type Z = Int

class Notation a where
  notation :: a -> Text

data Interval
  = EI { l :: Z }
  | DI { l :: Z }
  deriving (Show, Eq, Ord)

instance Notation Z where
  notation l = T.pack (show l)

instance Notation Interval where
  notation (EI l) = "[" <>  notation l <> "," <> (notation $ l+1) <> "]"
  notation (DI l) = "[" <> notation l <> "]"

newtype Cube = Cube { intervals :: Vector Interval }

instance Notation Cube where
  notation (Cube is) = ""
