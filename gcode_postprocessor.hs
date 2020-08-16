import System.Environment
import LayerInfo

runCommand :: String -> [String] -> IO ()
runCommand "layerinfo" = layerInfo
runCommand _ = \_ -> fail "Invalid command"


main :: IO ()
main = do
  args <- getArgs
  let cmd = args !! 0
  let rest = drop 1 args

  if length args > 0
    then runCommand cmd rest
    else fail "Command not supplied"
