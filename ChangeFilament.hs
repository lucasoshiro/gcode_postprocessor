module ChangeFilament where
import GCode
import Data.List
import Data.Set (Set, member)

changeFilament :: Set Layer -> GCode -> GCode
changeFilament layers elements = elements >>= handleEl
  where handleEl :: GCodeElement -> GCode
        handleEl el@(";":rest:_) =
          if "LAYER:" `isPrefixOf` rest then [el] ++ insertM600 rest else [el]
        handleEl el = [el]

        insertM600 :: String -> GCode
        insertM600 comment =
          if layer `member` layers then [["M600"]] else []
          where layer = read $ drop (length "LAYER:") comment
