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

import Data.Either.Extra
import Data.List
import Debug.Trace
import Text.Parsec
import Text.Printf

type Label = String
data Result = Result {
    name :: String,
    labelNr :: Int,
    truePositive :: Int,
    trueNegative :: Int,
    falsePositive :: Int,
    falseNegative :: Int,
    accuracy :: Double,
    precision :: Double,
    recall :: Double
}

anomaly = "1" :: String
nominal = "0" :: String

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

toResult :: String -> Int -> (Double, Double, Double, Double) -> Result
toResult name nr (tp, tn, fp, fn) =  
    Result {
        name = name,
        labelNr = nr,
        truePositive = round tp,
        trueNegative = round tn,
        falsePositive = round fp,
        falseNegative = round fn,
        accuracy = (tp + tn) / (tp + tn + fp + fn),
        precision = tp / (tp + tn),
        recall = tp / (tp + fp) 
    } 

instance Show Result where
    show r = unlines [
        name r ++ ": ",
        "LabelNr: " ++ show (labelNr r),
        "True Positive: " ++ show (truePositive r),
        "True Negative: " ++ show (trueNegative r),
        "False Positive: " ++ show (falsePositive r),
        "False Negative: " ++ show (falseNegative r),
        "Accuracy: " ++ printf doubleFormat (accuracy r),
        "Precision: " ++ printf doubleFormat (precision r),
        "Recall: " ++ printf doubleFormat (recall r)
        ]
        where doubleFormat = "%.4f"
        

compareMethods :: [[Label]] -> [Result]
compareMethods ds = map evalMethod $ tail ds
    where
        baseTruth = tail (head ds)

        evalMethod :: [Label] -> Result
        evalMethod (name:labels) = 
            toResult name (length labels)
            . foldl f (0, 0, 0, 0) 
            . zip baseTruth $ labels

        f (tp, tn, fp, fn) (t, p)
            | t == anomaly && p == anomaly = (tp+1, tn, fp, fn)
            | t == nominal && p == nominal = (tp, tn+1, fp, fn)
            | t == nominal && p == anomaly = (tp, tn, fp+1, fn)
            | t == anomaly && p == nominal = (tp, tn, fp, fn+1)

main :: IO ()
main = do
    myData <- readCSV <$> readFile "test.csv"
    mapM_ (putStrLn . show) (compareMethods myData)
