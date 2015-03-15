 
module GitCommands(currentAuthors, currentBranch, Authors, Branch) where
import           System.Process             (readProcessWithExitCode)
import Data.Char(isSpace)
import qualified Data.Text as T

type Branch = T.Text

type Authors = String

rstrip = reverse . dropWhile isSpace . reverse
currentAuthors :: IO Authors
currentAuthors = do
  (_, authors, _) <- readProcessWithExitCode "git" ["config", "user.name"] ""
  return $ rstrip authors

currentBranch :: IO Branch
currentBranch = do
  (_, branch, _) <- readProcessWithExitCode "git" ["symbolic-ref", "--short", "HEAD"] ""
  return $ T.pack $ rstrip branch

