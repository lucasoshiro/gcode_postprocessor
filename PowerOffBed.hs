module PowerOffBed where
import GCode

powerOffBed :: Layer -> GCode -> GCode
powerOffBed layer elements = elements >>= handleEl
  where handleEl :: GCodeElement -> GCode
        handleEl el =
          if isLayerComment el
          then insertM140 el
          else [el]

        insertM140 :: GCodeElement -> GCode
        insertM140 el = if layer' == layer then [el, m140] else [el]
          where layer' = head $ layerFromComment el
                m140 = ["M140", "S0"]

