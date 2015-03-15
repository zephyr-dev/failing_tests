{-# LANGUAGE OverloadedStrings #-}
module Types where
import qualified Data.HashMap.Strict as HM
import Data.Maybe (listToMaybe)
import Control.Monad(mzero)
import qualified Data.Vector as V
import Data.Vector((!?))
import qualified Data.ByteString.Lazy as BL
import Data.Aeson (decode, Value(..), FromJSON(..), (.:))
import qualified Data.Text as T
import Control.Applicative((<$>), (<*>), pure, (<|>))


data Steps = Steps { circleSteps :: [Step] } deriving(Show)
data Step = Step { stepName :: T.Text, stepActions :: [Action] } deriving(Show)
data Action = Action { containerNumber :: Integer,  actionFailed :: Bool, outputUrl :: T.Text } deriving(Show)

data Output = Output { message :: T.Text } deriving Show

data Build = Build { 
                buildId     :: Integer
              , status :: T.Text
              , branch :: T.Text
             } deriving (Show)


instance FromJSON Steps where
  parseJSON (Object v) = do 
    steps <- v .: "steps"
    Steps <$> mapM parseJSON steps
  parseJSON _  = mzero

instance FromJSON Step where
  parseJSON (Object v) = do 
    actions <- v .: "actions" 
    Step <$> v .: "name" <*> mapM parseJSON actions 
  parseJSON _  = mzero


instance FromJSON Action where
  parseJSON (Object v) = do 
    Action <$> v .: "index" <*>  (v .: "failed" <|> pure False ) <*> (v .: "output_url" <|> pure "")
  parseJSON _  = mzero

instance FromJSON Output where
  parseJSON output@(Array v) = do 
    op <- parseJSON output
    let elem = listToMaybe op
    case elem of 
         Just obj -> Output <$> obj .: "message"
         Nothing -> mzero
  parseJSON _  = mzero

instance FromJSON Build where
  parseJSON (Object v) = do
    Build <$> v .: "build_num" <*> v .: "outcome" <*> v .: "branch"
  parseJSON _  = mzero

data BuildCollection = BuildCollection { builds :: [Build] } deriving(Show)
instance FromJSON BuildCollection where
  parseJSON circleBuildCollection@(Array v) = do
    allBuildCollection <- parseJSON circleBuildCollection
    BuildCollection <$> mapM parseJSON allBuildCollection
  parseJSON _ =  mzero

decodeBuildCollection :: BL.ByteString -> Maybe BuildCollection
decodeBuildCollection = decode

decodeSteps :: BL.ByteString -> Maybe Steps
decodeSteps  = decode

decodeOutput :: BL.ByteString -> Maybe Output 
decodeOutput   = decode
