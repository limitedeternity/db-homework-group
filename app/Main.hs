{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Lib
import Control.Exception
import Text.Printf

import Control.Monad.Trans.Reader
import Options.Applicative
import Text.Show.Pretty (pPrint)
import Conduit
import Data.CSV.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import qualified Data.Map.Strict as Map


-- |
data CLIArgs = CLIArgs
    { csvPath :: FilePath
    , groupByColumn :: Int
    , aggregationColumn :: Int
    , aggregationFunction :: String
    }

cliArgsParser :: Parser CLIArgs
cliArgsParser = CLIArgs
    <$> argument str (metavar "CSV_FILE_PATH" <> help "Path to CSV file")
    <*> argument auto (metavar "GROUP_BY_COLUMN" <> help "Number of column to group by")
    <*> argument auto (metavar "AGGREGATION_COLUMN" <> help "Number of column to perform aggregation on")
    <*> argument str (metavar "AGGREGATION_FUNCTION" <> help "Aggregation function (max, min, sum, count)")

parseCLIArgs :: IO CLIArgs
parseCLIArgs = execParser $ info (cliArgsParser <**> helper) fullDesc


-- |
prepareCSVRow :: Row T.Text -> Reader CLIArgs (T.Text, Double)
prepareCSVRow row = do
    CLIArgs
        { csvPath = _
        , groupByColumn
        , aggregationColumn
        , aggregationFunction
        } <- ask

    case aggregationFunction of
      "count" -> return (row !! groupByColumn, 0 / 0)
      _       -> return (row !! groupByColumn, parseDouble $ row !! aggregationColumn)
          where
              parseDouble aggRow = either (const $ raiseTypeError aggRow) fst . TR.double $ aggRow
              raiseTypeError = throw . TypeError . printf "parseDouble used on \"%s\""

main :: IO ()
main = do
    args@CLIArgs
        { csvPath
        , groupByColumn = _
        , aggregationColumn = _
        , aggregationFunction
        } <- parseCLIArgs

    result <- runConduitRes $
        sourceFile csvPath
        .| intoCSV defCSVSettings
        .| CL.map (flip runReader args . prepareCSVRow)
        .| getAggregate aggregationFunction

    pPrint . Map.toList $ result
