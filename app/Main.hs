module Main where

import Data.Char
import Data.Csv
import System.IO (hPutStrLn, stderr)
import Ucrules
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Vector as V

main :: IO ()
main = do
    let tsvDecOs = defaultDecodeOptions {decDelimiter = fromIntegral (ord '\t')} 
    let tsvEncOs = defaultEncodeOptions {encDelimiter = fromIntegral (ord '\t')} 
    csvData <- BL8.getContents
    case decodeByNameWith tsvDecOs csvData :: Either String (Header, V.Vector CodRow) of
        Left err -> hPutStrLn stderr err
        Right (_, v) -> do
            let crnews = crnew <$> V.toList v
            BL8.putStr $ encodeDefaultOrderedByNameWith tsvEncOs crnews
