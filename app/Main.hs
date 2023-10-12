{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Lib
import Control.Exception
import Control.Monad
import Data.Semigroup
import System.IO
import Text.Printf
import qualified Data.List as L

import Control.Monad.Trans.Reader
import Options.Applicative
import Conduit
import Data.CSV.Conduit
import Data.Conduit.Algorithms
import System.Directory
import System.FilePath
import System.IO.Temp
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import qualified Data.Text.Lazy as T (toStrict)
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy.Builder.RealFloat as B
import qualified Data.Text.Read as TR
import qualified Data.Map.Strict as Map


-- |
data CLIArgs = CLIArgs
    { csvPath :: FilePath
    , groupByColumn :: Int
    , aggregationColumn :: Int
    , aggregationFunction :: String
    }

parseCLIArgs :: IO CLIArgs
parseCLIArgs = execParser $ info (cliArgsParser <**> helper) fullDesc
    where
        cliArgsParser :: Parser CLIArgs
        cliArgsParser = do
            csvPath <- argument str (metavar "CSV_FILE_PATH" <> help "Path to CSV file")
            groupByColumn <- argument auto (metavar "GROUP_BY_COLUMN" <> help "Number of column to group by")
            aggregationColumn <- argument auto (metavar "AGGREGATION_COLUMN" <> help "Number of column to perform aggregation on")
            aggregationFunction <- argument str (metavar "AGGREGATION_FUNCTION" <> help "Aggregation function (max, min, sum, count)")

            return $ CLIArgs
                csvPath
                (groupByColumn - 1)
                (aggregationColumn - 1)
                aggregationFunction


-- |
csvIntoSortedChunks :: Int -> ReaderT CLIArgs IO (FilePath, [FilePath])
csvIntoSortedChunks rowsPerChunk = do
    CLIArgs
        { csvPath
        , groupByColumn
        , aggregationColumn
        , aggregationFunction = _
        } <- ask

    systemTmpDir <- liftIO getCanonicalTemporaryDirectory
    liftIO $ bracketOnError
        (createTempDirectory systemTmpDir "groupby-exe") removePathForcibly (
            \tmpDir -> runConduitRes $
                sourceFile csvPath
                .| intoCSV defCSVSettings
                .| CC.map (\(row :: Row T.Text) -> [row !! groupByColumn, row !! aggregationColumn])
                .| CL.chunksOf rowsPerChunk
                .| CC.mapM (liftIO . writeChunk tmpDir)
                .| do
                    chunkFiles <- sinkList
                    return (tmpDir, chunkFiles))

        where
            writeChunk :: FilePath -> [Row T.Text] -> IO FilePath
            writeChunk tmpDir chunk = bracketOnError
                (emptyTempFile tmpDir "chunk-xxx.csv") removeFile (
                    \tmpFile -> runConduitRes $
                        CL.sourceList (L.sortOn head chunk)
                        .| fromCSV defCSVSettings
                        .| do
                            sinkFile tmpFile
                            return $! tmpFile)


-- |
prepareChunkRow :: Row T.Text -> Reader CLIArgs (Arg T.Text Double)
prepareChunkRow [groupColValue, aggColValue] = do
    CLIArgs
        { csvPath = _
        , groupByColumn = _
        , aggregationColumn = _
        , aggregationFunction
        } <- ask

    case aggregationFunction of
      "count" -> return $! Arg groupColValue (0 / 0)
      _       -> return $! Arg groupColValue (textToDouble aggColValue)
          where
              textToDouble text = either (const $ raiseTypeError text) fst . TR.double $ text
              raiseTypeError = throw . TypeError . printf "This doesn't look like a Double: \"%s\""

prepareChunkRow _ = throw . PatternMatchFail $ "prepareChunkRow used on unexpected input"

aggregateBySortedChunks :: (FilePath, [FilePath]) -> ReaderT CLIArgs IO FilePath
aggregateBySortedChunks (tmpDir, chunkFiles) = do
    args@CLIArgs
        { csvPath
        , groupByColumn = _
        , aggregationColumn = _
        , aggregationFunction
        } <- ask

    liftIO $ bracketOnError
        (emptyTempFile tmpDir "group-xxx.csv") removeFile (
            \tmpFile -> do
                withFile tmpFile WriteMode (
                    \hFile -> do
                        hSetBuffering hFile NoBuffering

                        leftoverMap <- runConduitRes $
                            mergeC [
                                sourceFile chunkFile
                                .| intoCSV defCSVSettings
                                .| CC.map (flip runReader args . prepareChunkRow)
                                 | chunkFile <- chunkFiles
                            ]
                            .| CC.foldM ((liftIO .) . aggregateToFile hFile (getFoldAggregate aggregationFunction)) Map.empty

                        appendMapAsCSV hFile leftoverMap)

                let resultFilePath = takeDirectory csvPath </> takeFileName tmpFile
                renameFile tmpFile resultFilePath
                return $! resultFilePath)

        where
            aggregateToFile ::
                Handle ->
                    (Map.Map T.Text Double -> (T.Text, Double) -> Map.Map T.Text Double) ->
                        Map.Map T.Text Double ->
                            Arg T.Text Double ->
                                IO (Map.Map T.Text Double)
            aggregateToFile hFile foldAgg foldAccMap (Arg groupColValue aggColValue) =
                if Map.member groupColValue foldAccMap
                   then return $! foldAgg foldAccMap (groupColValue, aggColValue)
                   else do
                       appendMapAsCSV hFile foldAccMap
                       return $! foldAgg Map.empty (groupColValue, aggColValue)

            appendMapAsCSV :: Handle -> Map.Map T.Text Double -> IO ()
            appendMapAsCSV hFile foldAccMap =
                unless (Map.null foldAccMap)
                (runConduit $
                    CL.sourceList (Map.toList foldAccMap)
                    .| CC.map (\(k, v) -> [k, realFloatToText v])
                    .| fromCSV defCSVSettings
                    .| sinkHandle hFile)

                where
                    realFloatToText :: RealFloat a => a -> T.Text
                    realFloatToText = T.toStrict . B.toLazyText . B.realFloat


-- |
main :: IO ()
main = do
    args@CLIArgs
        { csvPath
        , groupByColumn
        , aggregationColumn
        , aggregationFunction = _
        } <- parseCLIArgs

    headRowLength <- runConduitRes $
        sourceFile csvPath
        .| intoCSV defCSVSettings
        .| CC.map (\(row :: Row T.Text) -> [row !! groupByColumn, row !! aggregationColumn])
        .| CC.take 1
        .| fromCSV defCSVSettings
        .| CC.decodeUtf8Lenient
        .| do CC.line CC.lengthE

    let maxChunkSize = 500 * ((^) :: Int -> Int -> Int) 2 20 -- 500 MB
    let rowsPerChunk = maxChunkSize `div` headRowLength

    resultFilePath <- bracket
        (flip runReaderT args . csvIntoSortedChunks $ rowsPerChunk)
        (removePathForcibly . fst)
        (flip runReaderT args . aggregateBySortedChunks)

    putStrLn . printf "CSV table is written to: %s" $ resultFilePath
