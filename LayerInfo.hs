module LayerInfo where
import GCode
import Data.List

layerInfo :: [String] -> IO ()
layerInfo (input:output:_) = do
  raw <- readFile input
  let raw_lines = lines raw
  let parsed = gCodeFromStrings raw_lines
  let processed = insertLayerInfo parsed

  writeFile output $ stringFromGCode processed

layerInfo _ = fail "Missing arguments"

insertLayerInfo :: GCode -> GCode
insertLayerInfo elements = elements >>= handleEl
  where handleEl :: GCodeElement -> GCode
        handleEl el@(";":rest:_) =
          if "LAYER:" `isPrefixOf` rest
          then [el, insertM117 rest]
          else [el]
        handleEl el = [el]

        insertM117 :: String -> GCodeElement
        insertM117 comment = ["M117", msg]
          where msg = "Layer " ++ layer
                layer = drop (length "LAYER:") comment

                
