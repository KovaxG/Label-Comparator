{-
    Compare each column with the first one, and compute
    precision and recall for each labeling.

    CSV format:
    first row contains the names of each method
    the following rows are the classifications per datapoint

    The labels are either 0 or 1, where 
    0 is nominal and 1 is anomaly.
-}

-- {-# LANGUAGE Woverlapping-patters #-}

module Main where

import Data.Either.Extra
import Data.List
import Data.List.Zipper
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
    recall :: Double,
    adcai :: Double,
    myMethod :: Double
}

instance Show Result where
    show r = unlines [
        name r ++ ": ",
        "LabelNr: " ++ show (labelNr r),
        "True Positive: " ++ show (truePositive r),
        "True Negative: " ++ show (trueNegative r),
        "False Positive: " ++ show (falsePositive r),
        "False Negative: " ++ show (falseNegative r),
        "Accuracy: " ++ showDouble (accuracy r),
        "Precision: " ++ showDouble (precision r),
        "Recall: " ++ showDouble (recall r),
        "Average Distance to Closest Anomaly Identified: " ++  showDouble (adcai r),
        "MyMethod: " ++ showDouble (myMethod r)
        ]
        where showDouble = printf "%.4f"

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

toResult :: String -> Int -> [(Label, Label)] -> (Double, Double, Double, Double) -> Result
toResult name nr pairs (tp, tn, fp, fn) =  
    Result {
        name = name,
        labelNr = nr,
        truePositive = round tp,
        trueNegative = round tn,
        falsePositive = round fp,
        falseNegative = round fn,
        accuracy = (tp + tn) / (tp + tn + fp + fn),
        precision = tp / (tp + tn),
        recall = tp / (tp + fp),
        adcai = calc_adcai pairs,
        myMethod = 0
    } 

    
calc_adcai :: [(Label, Label)] -> Double
calc_adcai = mean . foldlz rule [] . fromList
    where
        mean :: [Int] -> Double
        mean is = sum ds / genericLength ds
            where ds = map fromIntegral is
    
        rule :: [Int] -> Zipper (Label, Label) -> [Int]
        rule ds z = 
            if isAnomaly fst (cursor z) 
            then distanceToClosestAnomaly z : ds
            else ds
            where
                isAnomaly :: ((Label, Label) -> Label) -> (Label, Label) -> Bool
                isAnomaly f p = f p == anomaly
                
                distanceToClosestAnomaly :: Zipper (Label, Label) -> Int
                distanceToClosestAnomaly z = min (go left beginp z 0) (go right endp z 0)
                    where
                        bignum = 1000000
                        go dir end z d = 
                            if maybe True (\c -> isAnomaly snd c) (safeCursor z) then d
                            else if end z then bignum
                                 else go dir end (dir z) (d+1)
                                    
compareMethods :: [[Label]] -> [Result]
compareMethods ds = map evalMethod $ tail ds
    where
        baseTruth = tail (head ds)

        evalMethod :: [Label] -> Result
        evalMethod (name:labels) = 
            toResult name (length labels) pairs
            . foldl f (0, 0, 0, 0) $ pairs
            where 
                pairs = zip baseTruth labels

        f (tp, tn, fp, fn) (t, p)
            | t == anomaly && p == anomaly = (tp+1, tn, fp, fn)
            | t == nominal && p == nominal = (tp, tn+1, fp, fn)
            | t == nominal && p == anomaly = (tp, tn, fp+1, fn)
            | t == anomaly && p == nominal = (tp, tn, fp, fn+1)

main :: IO ()
main = do
    myData <- readCSV <$> readFile "test.csv"
    mapM_ (putStrLn . show) (compareMethods myData)
