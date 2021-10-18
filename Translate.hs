import GCode
import System.Environment

type Position = (Maybe Float, Maybe Float, Maybe Float)

main :: IO ()
main = do
  input:output:_x:_y:_z:_ <- getArgs

  let x:y:z:_ = map read [_x, _y, _z] :: [Float]

  gcode <- readGCodeFile input
  let new = translateGCode (x, y, z) gcode

  writeFile output $ stringFromGCode new


translateElement :: (Float, Float, Float) -> GCodeElement -> GCodeElement
translateElement (dx, dy, dz) el =
  if (length el > 0) && (cmd == "G0" || cmd == "G1") then new_el else el
  where
    cmd:params = el

    parse_axis = read . tail

    old_x = [parse_axis p | p <- params, head p == 'X']
    old_y = [parse_axis p | p <- params, head p == 'Y']
    old_z = [parse_axis p | p <- params, head p == 'Z']

    other = [ p
            | p <- params
            , let c = head p in c /= 'X' && c /= 'Y' && c /= 'Z'
            ]

    new_x = old_x >>= return . (+ dx)
    new_y = old_y >>= return . (+ dy)
    new_z = old_z >>= return . (+ dz)

    unparse_axis c = (c:) . show

    new_position_params =
      [ new_x >>= return . unparse_axis 'X'
      , new_y >>= return . unparse_axis 'Y'
      , new_z >>= return . unparse_axis 'Z'
      ] >>= id

    new_el = [cmd] ++ new_position_params ++ other


translateGCode :: (Float, Float, Float) -> GCode -> GCode
translateGCode t = map $ translateElement t
