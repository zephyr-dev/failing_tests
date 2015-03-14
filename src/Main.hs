{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad(mzero)
{- import GitCommands(currentAuthors, currentBranch, Authors) -}
import Control.Lens
import Network.Wreq
import qualified Data.ByteString.Lazy as BL
import Control.Applicative(pure, (<$>), (<*>), (<|>))
import Data.Aeson (Value(..), decode, encode, FromJSON(..), (.:))
{- import Data.Map (Map) -}
{- import qualified Data.Vector as V -}
import qualified Data.Text as T
buildsUrl :: String
buildsUrl = "https://circleci.com/api/v1/project/zephyr-dev/gust?circle-token=a5422c509e6c049514733030174a901e8cd17b3e&filter=completed"

buildUrl :: Integer -> String
buildUrl num = "https://circleci.com/api/v1/project/zephyr-dev/gust/" ++ (show num) ++ "?circle-token=a5422c509e6c049514733030174a901e8cd17b3e"


data Build = Build { 
                id     :: Integer
              , status :: T.Text
              , author :: T.Text
              , branch :: T.Text
             } deriving (Show)

instance FromJSON Build where
  parseJSON (Object v) = do
    Build <$> v .: "build_num" <*> v .: "outcome" <*> (v .: "author_name" <|> pure "") <*> v .: "branch"
  parseJSON _  = mzero

data Builds = Builds { builds :: [Build] } deriving(Show)
instance FromJSON Builds where
  parseJSON circleBuilds@(Array v) = do
    allBuilds <- parseJSON circleBuilds
    Builds <$> mapM parseJSON allBuilds
  parseJSON _ =  mzero


decodeBuilds :: BL.ByteString -> Maybe Builds
decodeBuilds = decode

opts = defaults & header "Accept" .~ ["application/json"]
main :: IO ()
main = do
  response <- getWith opts "https://circleci.com/api/v1/project/zephyr-dev/gust?circle-token=a5422c509e6c049514733030174a901e8cd17b3e&filter=completed"
  putStrLn . show $  decodeBuilds $ (response ^. responseBody)
