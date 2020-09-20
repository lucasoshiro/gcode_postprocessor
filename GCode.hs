module GCode where
import Data.List


type GCode = [GCodeElement]
type GCodeElement = [String]
type Layer = Int
type FeedRate = Int


gCodeFromStrings :: [String] -> GCode
gCodeFromStrings = map parseGCodeElement


parseGCodeElement :: String -> GCodeElement
parseGCodeElement (';':rest) = [";", rest]
parseGCodeElement s = words s


stringFromGCode :: GCode -> String
stringFromGCode = intercalate "\n" . map stringFromElement
  where stringFromElement (";":rest) = ";" ++ intercalate " " rest
        stringFromElement el = intercalate " " el


isLayerComment :: GCodeElement -> Bool
isLayerComment (";":rest:_) = "LAYER:" `isPrefixOf` rest
isLayerComment _ = False


layerFromComment :: GCodeElement -> [Int]
layerFromComment (";":c:_) = [read $ drop (length "LAYER:") c]
layerFromComment _ = []


isParamComment :: GCodeElement -> Bool
isParamComment (";":rest:_) = "POSTPROCESS" `isPrefixOf` rest
isParamComment _ = False


paramsFromComment :: GCodeElement -> [String]
paramsFromComment (";":rest:_) = words $ drop (length "POSTPROCESS") rest
paramsFromComment _ = []
