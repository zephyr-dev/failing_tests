{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad(mzero)
import qualified Data.ByteString.Lazy as BL
import Control.Exception(catch)
import GHC.Exception(Exception(..))
import Control.Concurrent (threadDelay)
import Control.Applicative((<$>))
import Control.Lens ((.~), (&), (^.), _head, (^?), filtered)
import Data.Aeson (Value(..), decode, encode, FromJSON(..), (.:))
import Data.Aeson.Lens (key, _String, _Array)
import Network.Wreq (Options, get, getWith, defaults, header, Response, asJSON, responseBody)
import Data.Map (Map)
import qualified Data.Vector as V
import Data.Char(isSpace)
import           System.Process             (readProcessWithExitCode)
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import Data.List 
import System.Directory (getCurrentDirectory, setCurrentDirectory)
import Network.HTTP.Conduit(HttpException, simpleHttp)

buildsUrl :: String
buildsUrl = "https://circleci.com/api/v1/project/zephyr-dev/gust?circle-token=a5422c509e6c049514733030174a901e8cd17b3e&filter=completed"

buildUrl :: Integer -> String
buildUrl num = "https://circleci.com/api/v1/project/zephyr-dev/gust/" ++ (show num) ++ "?circle-token=a5422c509e6c049514733030174a901e8cd17b3e"


type Body = Value

opts = defaults & header "Accept" .~ ["application/json"]


type Authors = String
type Branch = String
type Build = Value
type Step = Value

data GustBuild = GustBuild { buildNum :: Integer } deriving(Show)
instance FromJSON GustBuild where
  parseJSON (Object v) = GustBuild <$> (v .: "build_num")
  parseJSON _ = mzero


f :: HttpException -> IO (Either String b)
f e = return $ Left $ show e


decodeGustBuild :: BL.ByteString -> Maybe GustBuild
decodeGustBuild = decode

rstrip = reverse . dropWhile isSpace . reverse

failingTestUrls :: Build -> IO (Either String [T.Text])
failingTestUrls  b = do
  buildInfo <- getBuildInfo $ parseBuildNum b
  case buildInfo of
    Right r -> return $ Right $ urlsForFailedBuilds r
    Left s    -> return $ Left s
  where 
    urlsForFailedBuilds   :: Response Body -> [T.Text]
    urlsForFailedBuilds  r = concat $ V.toList $ V.filter (\x -> (length x) > 0) $ V.map outputUrl $ r ^. responseBody . key "steps" . _Array
      where
    outputUrl :: Step -> [T.Text]
    outputUrl step =  V.toList $ V.map findUrl $ V.filter failedSteps $ step ^. key "actions" . _Array
      where 
        failedSteps :: Step -> Bool
        failedSteps s = (s ^. key "status" . _String) == "failed"
        findUrl :: Value -> T.Text
        findUrl action = action ^. key "output_url" . _String

    getBuildInfo :: Maybe Integer -> IO (Either String (Response Body))
    getBuildInfo (Just num) = safeRequest $ buildUrl num
    parseBuildNum build = buildNum <$> (decodeGustBuild $ encode build)



data CircleBuildOutput = CircleBuildOutput { 
  circleOutputMessages :: T.Text
} deriving (Show)

instance FromJSON CircleBuildOutput where
  parseJSON arr@(Array v) = do
    array <- parseJSON arr
    CircleBuildOutput <$> (head array) .: "message"
  parseJSON _          = mzero



decodeCircleStdout :: BL.ByteString -> Maybe CircleBuildOutput
-- TODO make this an instance of toJSON
decodeCircleStdout = decode

buildFailures :: Build -> IO ()
buildFailures a = do 
  failingMessageUrls <- failingTestUrls  a
  case (failingMessageUrls) of
    Right b -> printOutput b
    Left c -> putStrLn $ "Something went wrong when trying to access the circle api! " ++ c

    where
      printOutput :: [T.Text] -> IO ()
      printOutput xs = do 
        responses <- mapM simpleHttp (map T.unpack xs)
        let responseMessages = map decodeCircleStdout responses
        putStrLn $ show responseMessages 
        return ()

type Url = String
safeRequest :: Url -> IO (Either String (Response Body))
safeRequest url = catch (return . Right =<< asJSON =<< getWith opts url) f

main :: IO ()
main = do
  path <- getCurrentDirectory
  eitherResult <- safeRequest buildsUrl
  case eitherResult of
    Right r -> do
      authors <- currentAuthors
      let currentCompletedBuilds = V.toList $ V.filter (filterByAuthors authors) $ r ^. responseBody . _Array
      let mostRecentBuild = listToMaybe currentCompletedBuilds
      case mostRecentBuild of
        Nothing -> putStrLn $ "Could not find any recent builds for: " ++ authors
        Just a  -> buildFailures a
    Left err -> do
      return ()
    where
      filterByAuthors :: Authors -> (Build -> Bool)
      filterByAuthors authors build = build ^. key "author_name" . _String == T.pack authors

currentAuthors :: IO Authors
currentAuthors = do
  (_, authors, _) <- readProcessWithExitCode "git" ["config", "user.name"] ""
  {- putStrLn $ "current authors: " ++ authors  -}
  return $ rstrip authors

currentBranch :: IO Branch
currentBranch = do
  (_, branch, _) <- readProcessWithExitCode "git" ["symbolic-ref", "--short", "HEAD"] ""
  {- putStrLn $ "current branch: " ++ branch  -}
  return $ rstrip branch
