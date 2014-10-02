
import System.Process

main :: IO ()
main = do
  rawSystem "scp" ["dist/build/migration-app/migration-app","root@75.126.55.87:migration"]
  return ()
