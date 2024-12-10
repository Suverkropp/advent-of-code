module Y2015.Day08 (day8, parseString) where

import AoC
import Data.Char (chr)

day8 :: AoC String
day8 =
  AoC
    { year = 2015,
      day = 8,
      handleInput = id,
      part1 = sum . map getLengthDiff . lines,
      part2 = sum . map getEncodeLength . lines
    }

parseString :: String -> String
parseString ('\"':s) = parseRest s
parseString s = error "No starting \" in :" ++ show s

parseRest :: String -> String
parseRest ('\\':'\\':s) = '\\' : parseRest s
parseRest ('\\':'\"':s) = '\"' : parseRest s
parseRest ('\\':'x':d1:d2:s) = chr (read ['0','x',d1,d2]) : parseRest s
parseRest "\"" = ""
parseRest (c:s) = c:parseRest s
parseRest ""= error "no closing qoutes"

getLengthDiff :: String -> Int
getLengthDiff s = length s - length (parseString s)

encodeString :: String -> String
encodeString s = "\"" ++ concatMap encodeChar s ++ "\""

encodeChar :: Char -> String
encodeChar '\\' = "\\\\"
encodeChar '\"' = "\\\""
encodeChar c = [c]

getEncodeLength :: String -> Int
getEncodeLength s = length (encodeString s) - length s
