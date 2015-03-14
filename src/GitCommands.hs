 
module GitCommands(currentAuthors, currentBranch, Authors) where
import           System.Process             (readProcessWithExitCode)
import Data.Char(isSpace)
type Branch = String

type Authors = String

rstrip = reverse . dropWhile isSpace . reverse
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

{
  :previous_successful_build {
    :build_time_millis 1893512, 
    :status :fixed, 
    :build_num 12170
  }, 
  :committer_name "Peter Swan", 
  :circle-yml { 
    :string ""
   , 
  :infrastructure_fail false, 
  :job-name nil, 
  :why "github", 
  :parallel 14, 
  :retries nil, 
  :committer_email "zephyr-eng+pdswan@gust.com", 
  :build-parameters nil, 
  :username "zephyr-dev", 
  :start_time #<DateTime 2015-03-13T22:14:15.668Z>, 
  :ssh-enabled nil, 
  :build_num 12190, 
  :timedout false, 
  :stop_time #<DateTime 2015-03-13T22:42:03.896Z>, 
  :author_date "2015-03-13T17:52:48-04:00", 
  :dont-build nil, 
  :build_time_millis 1668228, 
  :outcome :failed, 
  :all-commit-details [
    {
      :commit_url "https://github.com/zephyr-dev/gust/commit/723f91f0f308a25c3886fd65f96abd0484c242b6", 
      :committer_name "Peter Swan", 
      :committer_email "zephyr-eng+pdswan@gust.com", 
      :author_date "2015-03-13T17:52:48-04:00", 
      :author_login nil, 
      :committer_login nil, 
      :commit "723f91f0f308a25c3886fd65f96abd0484c242b6", 
      :author_name "Peter Swan", 
      :author_email "zephyr-eng+pdswan@gust.com", 
      :committer_date "2015-03-13T17:52:48-04:00", 
      :branch "rails4-upgrade-v11", 
      :body "", 
      :subject "There's no alert to confirm [#85806714]"}
      ], 
  :lifecycle :finished, 
  :node ({:public-ip-addr "54.87.53.220", :port 64578, :username "ubuntu", :image-id "circletar-0459-474af-20150227T175004Z", :ssh-enabled nil} {:public-ip-addr "54.167.81.35", :port 64595, :username "ubuntu", :image-id "circletar-0459-474af-20150227T175004Z", :ssh-enabled nil} {:public-ip-addr "23.22.190.59", :port 64673, :username "ubuntu", :image-id "circletar-0459-474af-20150227T175004Z", :ssh-enabled nil} {:public-ip-addr "54.234.221.198", :port 64623, :username "ubuntu", :image-id "circletar-0459-474af-20150227T175004Z", :ssh-enabled nil} {:public-ip-addr "54.147.158.132", :port 64627, :username "ubuntu", :image-id "circletar-0459-474af-20150227T175004Z", :ssh-enabled nil} {:public-ip-addr "54.226.107.75", :port 64546, :username "ubuntu", :image-id "circletar-0459-474af-20150227T175004Z", :ssh-enabled nil} {:public-ip-addr "23.22.190.59", :port 64682, :username "ubuntu", :image-id "circletar-0459-474af-20150227T175004Z", :ssh-enabled nil} {:public-ip-addr "54.234.221.198", :port 64766, :username "ubuntu", :image-id "circletar-0459-474af-20150227T175004Z", :ssh-enabled nil} {:public-ip-addr "54.147.158.132", :port 64719, :username "ubuntu", :image-id "circletar-0459-474af-20150227T175004Z", :ssh-enabled nil} {:public-ip-addr "54.226.107.75", :port 64711, :username "ubuntu", :image-id "circletar-0459-474af-20150227T175004Z", :ssh-enabled nil} {:public-ip-addr "23.22.190.59", :port 64608, :username "ubuntu", :image-id "circletar-0459-474af-20150227T175004Z", :ssh-enabled nil} {:public-ip-addr "54.234.221.198", :port 64604, :username "ubuntu", :image-id "circletar-0459-474af-20150227T175004Z", :ssh-enabled nil} {:public-ip-addr "54.147.158.132", :port 64599, :username "ubuntu", :image-id "circletar-0459-474af-20150227T175004Z", :ssh-enabled nil} {:public-ip-addr "54.226.107.75", :port 64587, :username "ubuntu", :image-id "circletar-0459-474af-20150227T175004Z", :ssh-enabled nil}), :retry-of nil, :vcs_revision "723f91f0f308a25c3886fd65f96abd0484c242b6", :canceled false, :build_url "https://circleci.com/gh/zephyr-dev/gust/12190", :messages (), :vcs_url "https://github.com/zephyr-dev/gust", :status :failed, :previous {:build_time_millis 1683850, :status :failed, :build_num 12187}, :compare "https://github.com/zephyr-dev/gust/compare/b3c7beb65ff2...723f91f0f308", :author_name "Peter Swan", :author_email "zephyr-eng+pdswan@gust.com", :queued-at #<DateTime 2015-03-13T22:14:14.448Z>, :feature-flags {:build_GH1157_container_oriented_ui true}, :committer_date "2015-03-13T17:52:48-04:00", :oss false, :branch "rails4-upgrade-v11", :reponame "gust", :body "", :user {:is-user true, :login "zephyr-dev", :name nil, :email "zephyr-dev@googlegroups.com"}, :subject "There's no alert to confirm [#85806714]", :is-first-green-build false, :failed true, :usage-queued-at #<DateTime 2015-03-13T21:52:56.152Z>
     }
}
