module LayerInfo where
import GCode
import Data.List

layerInfo :: GCode -> GCode
layerInfo elements = elements >>= handleEl
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

                
