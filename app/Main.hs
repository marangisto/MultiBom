{-# LANGUAGE DeriveDataTypeable, RecordWildCards, OverloadedStrings #-}
module Main (main) where

import System.Console.CmdArgs
import System.FilePath
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import Control.Applicative
import GHC.Exts (groupWith)
import Data.List (sort, intercalate, findIndex)
import Data.Csv

data Options = Options
    { files :: [String]
    } deriving (Show, Eq, Data, Typeable)

options :: Main.Options
options = Main.Options { files = [] }

csvFileCount :: String -> (FilePath, Int)
csvFileCount s
    | Just i <- pos = (f $ take i s, read $ drop (i + 1) s)
    | otherwise = (f s, 1)
    where pos = findIndex (=='=') s
          f n = "/home/marten" </> n </> n <.> "csv"

data Item = Item
    { reference :: !String
    , quantity :: !Int
    , value :: !String
    , footprint :: !String
    , datasheet :: !String
    } deriving (Show)

instance FromNamedRecord Item where
    parseNamedRecord r = Item
        <$> r .: "Reference"
        <*> r .: " Quantity"
        <*> r .: " Value"
        <*> r .: " Footprint"
        <*> r .: " Datasheet"

loadCSV :: String -> IO [Item]
loadCSV s = do
    let (fn, count) = csvFileCount s
        mul Item{..} = Item{quantity = count * quantity, ..}
    csv <- BL.readFile fn
    return $ case decodeByName csv of
        Left e -> error e
        Right v -> map mul $ V.toList $ snd v

columns :: String
columns = intercalate "," [ "Footprint", "Value", "Quantity"]

format :: ((String, String), Int) -> String
format ((a, b), c) = intercalate "," [ show a, show b, show c ] 

asItem :: FilePath -> Item
asItem f = Item "" 1 f "Assembly" "~"

main :: IO ()
main = do
    opts@Options{..} <- cmdArgs options
    xs <- concat <$> mapM loadCSV files
    let ys = sort [ ((footprint, value), quantity) | Item{..} <- xs ++ map asItem files ]
        zs = [ (fst $ head ts, sum $ map snd ts) | ts <- groupWith fst ys ]
    putStrLn columns
    mapM_ (putStrLn . format) zs

