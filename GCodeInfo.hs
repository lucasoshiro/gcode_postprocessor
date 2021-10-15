import System.Environment
import GCode

type BRL = Float
type KG = Float
type MM = Float
type MG_MM3 = Float

main :: IO ()
main = do
  filename:material:_cost_kg:_ <- getArgs

  gcode <- readGCodeFile filename

  let cost_kg = read _cost_kg :: BRL

  let l = filamentLength gcode
  let _price = price material cost_kg l

  case _price of
    Just p -> putStrLn ("R$" ++ show p)
    Nothing -> fail "Unknown material"


filamentLength :: GCode -> Float
filamentLength gcode = last_pos - first_pos
  where
    -- take only G1s before G91
    until_G91 = takeWhile (/= ["G91"]) gcode
    g1s = [el | el <- until_G91, (head el) == "G1"]

    -- Extract extruder positions
    nonempty = (> 0) . length
    extruder_pos = map head . filter nonempty $
      [[read . tail $ param
       | param <- el, head param == 'E'
       ]
      | el <- g1s
      ]

    (first_pos, last_pos) = (head extruder_pos, last extruder_pos)


density :: String -> Maybe MG_MM3
density "PLA"   = return 1.24
density "PET-G" = return 1.27
density "PETG"  = return 1.27
density "ABS"   = return 1.04
density "TPU"   = return 1.21
density "ASA"   = return 1.05
density "HIPS"  = return 1.03
density _       = fail "Unknown material"


price :: String -> BRL -> MM -> Maybe BRL
price material cost_kg l = weight_kg >>= return . (* cost_kg)
  where
    diameter_mm = 1.75
    r = diameter_mm / 2
    _density_mg_mm3 = density material
    volume_mm3 = pi * r ** 2 * l
    weight_mg = _density_mg_mm3 >>= return . (* volume_mm3)
    weight_kg = weight_mg >>= return . (/ 1000000)
