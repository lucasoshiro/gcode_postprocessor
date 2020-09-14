module Action where
import GCode
import LayerInfo
import ChangeFilament
import PowerOffBed
import qualified Data.Set as S
import qualified Data.Map as M

type Action = GCode -> GCode
type Command = [String] -> Action

commands :: M.Map String Command
commands = M.fromList $
  [ ("layerinfo",      \_ -> layerInfo)
  , ("changefilament", changeFilament . S.fromList . map read)
  , ("poweroffbed",    powerOffBed . read . head)
  ]


parseArguments :: [String] -> Maybe (String, String, [Action])
parseArguments (input:output:args) = Just (input, output, callbacks)
  where parsedCommands = parseArguments' args
        callbacks = map (\(x:xs) -> (commands M.! x) xs) parsedCommands
parseArguments _ = Nothing


parseArguments' :: [String] -> [[String]]
parseArguments' [] = [] 
parseArguments' args = foldl update [] args
  where cmds = S.fromList $ M.keys commands
        update l arg =
          if arg `S.member` cmds then l ++ [[arg]]
          else (init l) ++ [(last l) ++ [arg]]


execute :: String -> String -> [Action] -> IO ()
execute input output actions = do
    raw <- readFile input
    let parsed = gCodeFromStrings $ lines raw
    let processed = foldl1 (.) actions $ parsed
    writeFile output $ stringFromGCode processed
  

parseAndExecute :: [String] -> IO ()
parseAndExecute s = do
  let parsed = parseArguments s
  case parsed of
    Nothing -> fail "Not enough arguments"
    Just (input, output, actions) -> execute input output actions
