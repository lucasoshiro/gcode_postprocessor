import Action
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  parseAndExecute args
