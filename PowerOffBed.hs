module PowerOffBed where
import GCode


powerOffBed :: [String] -> IO ()
powerOffBed (input:output:raw_layer:_) = do
  let layer = read raw_layer
  raw <- readFile input
  let raw_lines = lines raw
  let parsed = gCodeFromStrings raw_lines
  let processed = insertPowerOffBed parsed layer
  writeFile output $ stringFromGCode processed
powerOffBed _ = fail "Missing arguments"
      

insertPowerOffBed :: GCode -> Int -> GCode
insertPowerOffBed elements layer = elements >>= handleEl
  where handleEl :: GCodeElement -> GCode
        handleEl el =
          if isLayerComment el
          then insertM140 el
          else [el]

        insertM140 :: GCodeElement -> GCode
        insertM140 el = if layer' == layer then [el, m140] else [el]
          where layer' = head $ layerFromComment el
                m140 = ["M140", "S0"]

