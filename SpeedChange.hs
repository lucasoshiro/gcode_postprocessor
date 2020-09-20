module SpeedChange where
import GCode
import Data.Map hiding (drop)
import Data.List (isPrefixOf)

speedChange :: Map Layer FeedRate -> GCode -> GCode
speedChange layer_fr elements = elements >>= handleEl
  where handleEl :: GCodeElement -> GCode
        handleEl el@(";":rest:_) =
          if "LAYER:" `isPrefixOf` rest then [el] ++ insertM220 rest else [el]
        handleEl el = [el]

        insertM220 :: String -> GCode
        insertM220 comment =
          if layer `member` layer_fr
          then let fr = layer_fr ! layer
                   arg = 'S':(show fr)
               in [["M220", arg]]
          else []
          where layer = read $ drop (length "LAYER:") comment
