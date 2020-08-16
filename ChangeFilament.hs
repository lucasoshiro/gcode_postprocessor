module ChangeFilament where
import GCode
import Data.List
import Data.Set (Set, fromList, member)

type Layer = Int

changeFilament :: [String] -> IO ()
changeFilament (input:output:args) = do
  let layers = fromList $ map read args :: Set Layer
  raw <- readFile input
  let raw_lines = lines raw
  let parsed = gCodeFromStrings raw_lines
  let processed = insertChangeFilament parsed layers

  writeFile output $ stringFromGCode processed
changeFilament _ = fail "Missing arguments"


insertChangeFilament :: GCode -> Set Layer -> GCode
insertChangeFilament elements layers = elements >>= handleEl
  where handleEl :: GCodeElement -> GCode
        handleEl el@(";":rest:_) =
          if "LAYER:" `isPrefixOf` rest then [el] ++ insertM600 rest else [el]
        handleEl el = [el]

        insertM600 :: String -> GCode
        insertM600 comment =
          if layer `member` layers then [["M600"]] else []
          where layer = read $ drop (length "LAYER:") comment
