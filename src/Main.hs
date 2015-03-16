{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative(pure, (<$>), (<*>), (<|>))
import Types
import Control.Lens
import Data.Aeson (Value(..), decode, encode, FromJSON(..), (.:))
import qualified Data.ByteString.Lazy as BL
import Data.Maybe(listToMaybe)
import qualified Data.Text as T
import GitCommands(Branch, currentBranch)
import Network.Wreq
buildsUrl :: String
buildsUrl = "https://circleci.com/api/v1/project/zephyr-dev/gust?circle-token=a5422c509e6c049514733030174a901e8cd17b3e&filter=completed"

buildUrl :: Integer -> String
buildUrl num = "https://circleci.com/api/v1/project/zephyr-dev/gust/" ++ (show num) ++ "?circle-token=a5422c509e6c049514733030174a901e8cd17b3e"



opts = defaults & header "Accept" .~ ["application/json"]

filterByBranch :: Branch -> [Build] -> [Build]
filterByBranch branchName allBuilds = filter (forCurrentBranch branchName) allBuilds
  where 
    forCurrentBranch :: Branch -> Build -> Bool
    forCurrentBranch  myBranch build = myBranch == branch build

buildsForBranch :: Branch -> Maybe BuildCollection -> Maybe [Build]
buildsForBranch currBranch maybeBuiltCollection = do 
  buildCollection <- maybeBuiltCollection    
  return $ filterByBranch currBranch (builds buildCollection)

outputUrlsForFailedSteps :: Steps -> [T.Text]
outputUrlsForFailedSteps = outputUrlsForActions . failedActions . actionsInSteps 

failedActions :: [Action] -> [Action]
failedActions = filter actionFailed

actionsInSteps :: Steps -> [Action]
actionsInSteps steps = concat . map stepActions $ circleSteps steps

outputUrlsForActions  :: [Action]-> [T.Text]
outputUrlsForActions actions = map outputUrl actions

maybeListToMaybe :: Maybe [a] -> Maybe a
maybeListToMaybe x = x >>= listToMaybe

getCircleBuildCollection :: IO (Maybe BuildCollection)
getCircleBuildCollection = do 
  response <- getWith opts "https://circleci.com/api/v1/project/zephyr-dev/gust?circle-token=a5422c509e6c049514733030174a901e8cd17b3e&filter=completed"
  return . decodeBuildCollection $ (response ^. responseBody)



combineMessages :: Output -> String -> String
combineMessages out1 str = (T.unpack $ message out1) ++ str

failingSpecs :: [BL.ByteString] -> String
failingSpecs outputArray = do 
  let outputs = mapM decodeOutput outputArray
  case outputs of 
       Just xs -> foldr combineMessages "" xs
       Nothing -> "No output"

getit :: Maybe [T.Text] -> IO ()
getit mxs = case mxs of 
                 Just xs -> printMessagesForSteps xs Nothing -> return ()
    where
      printMessagesForSteps :: [T.Text] -> IO ()
      printMessagesForSteps buildUrls = do 
        outputResponses <- mapM responseBodyForUrl $ map T.unpack buildUrls 
        putStrLn $ failingSpecs outputResponses
          where 
            responseBodyForUrl :: String -> IO (BL.ByteString)
            responseBodyForUrl  url = do 
              res <- getWith opts url
              return $ res ^. responseBody

printBuildMessages :: Build ->  IO ()
printBuildMessages bld = do 
  response <- getWith opts $ buildUrl (buildId bld)
  putStrLn $ show $ outputUrlsForFailedSteps <$> (decodeSteps $ response ^. responseBody)


buildMessagesForLastFailure :: Branch -> Maybe [Build] -> IO ()
buildMessagesForLastFailure gitBranch blds = do
  case (maybeListToMaybe blds) of 
     Just circleBuild -> printBuildMessages circleBuild
     Nothing -> putStrLn $ "There were no builds for: " ++ (T.unpack gitBranch) ++ " in the last 30 builds on circle"

main :: IO ()
main = do
  circleBuildCollection <- getCircleBuildCollection
  currBr <- currentBranch
  buildMessagesForLastFailure currBr $ buildsForBranch currBr circleBuildCollection
