{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Common where

import qualified Data.Text as T
import Data.List
import Data.List.Split (splitOn)
import qualified Eventlog.Args as EA


{- Types -}
data ProfileLevel = OneLevel | TwoLevel deriving Show


{- Consts -}
svgPath :: String
svgPath = "tmp/graph.svg"

fontPath :: String
fontPath = "src/DejaVuSans.ttf"

{- Utils -}
truncN :: Int -> String -> String
truncN n s = take n s ++ (if length s > n then "..." else "")
trunc :: String -> String
trunc = truncN 30
truncT :: T.Text -> T.Text
truncT = T.pack . trunc . T.unpack


{- Encoders -}
encodePath :: [Int] -> T.Text
encodePath = T.pack . intercalate "-" . map show


{- Parsers -} 
parsePath :: String -> [Int]
parsePath [] = []
parsePath s = map read $ splitOn "-" s

parseProfileLevel :: String -> ProfileLevel
parseProfileLevel "1" = OneLevel
parseProfileLevel "2" = TwoLevel  
parseProfileLevel _ = error "Error: profile level not supported"

parseSort :: String -> EA.Sort
parseSort "size" = EA.Size
parseSort "stddev" = EA.StdDev
parseSort "name" = EA.Name
parseSort "number" = EA.Number
parseSort "gradient" = EA.Gradient
parseSort _ = error "invalid sort option"

