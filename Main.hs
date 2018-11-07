{-
    Compare each column with the first one, and compute
    precision and recall for each labeling.

    CSV format:
    first row contains the names of each method
    the following rows are the classifications per datapoint

    The labels are either 0 or 1, where 
    0 is nominal and 1 is anomaly.
-}
module Main where

import Data.Either
import Data.List
import Debug.Trace
import Text.Parsec

type Label = String
data Result = Result {
    name :: String,
    accuracy :: Double,
    recall :: Double
}  deriving (Show)

readCSV :: String -> [[Label]]
readCSV =
    transpose 
    . filter (/= [""]) 
    . fromRight [] 
    . parse rule "idk"
    where
        rule = sepBy line (char '\n')
        line = sepBy cols (char ',')
        cols = many alphaNum

toResult :: String -> (Double, Double, Double, Double) -> Result
toResult name (tp, tn, fp, fn) =  
    Result {
        name = name,
        accuracy = tp / (tp + tn),
        recall = tp / (tp + fp) 
    } 

compareMethods :: [[Label]] -> [Result]
compareMethods ds = map evalMethod $ tail ds
    where
        baseTruth = tail (head ds)

        evalMethod :: [Label] -> Result
        evalMethod (name:labels) = 
            toResult name
            . foldl f (0, 0, 0, 0) 
            . zip baseTruth $ labels

        f (tp, tn, fp, fn) (t, p)
            | t == "1" && p == "1" = (tp+1, tn, fp, fn)
            | t == "0" && p == "0" = (tp, tn+1, fp, fn)
            | t == "0" && p == "1" = (tp, tn, fp+1, fn)
            | t == "1" && p == "0" = (tp, tn, fp, fn+1)
         
main :: IO ()
main = do
    myData <- readCSV <$> readFile "test.csv"
    mapM_ (putStrLn . show) (compareMethods myData)
