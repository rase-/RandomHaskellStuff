module CSVParser
(parseCSV) where

import Text.Parsec
import Text.Parsec.String

-- Simple parser
csvFile :: GenParser Char st [[String]]
csvFile = do result <- many line
             eof
             return result

line :: GenParser Char st [String]
line = do result <- cells
          eol
          return result

cells :: GenParser Char st [String]
cells = do first <- cellContent
           next <- remainingCells
           return $ first : next

remainingCells :: GenParser Char st [String]
remainingCells = (char ',' >> cells)  <|> return []

cellContent :: GenParser Char st String
cellContent = many $ noneOf ",\n"

eol :: GenParser Char st Char
eol = char '\n'

parseCSV :: String -> Either ParseError [[String]]
parseCSV text = parse fCsvFile "(unknown)" text

-- Simpler implementation
sCsvFile = sLine `endBy` eol
sLine =  sCell `sepBy` char ','
sCell = many $ noneOf ",\n"

-- Full parser implementation
fCsvFile = fLine `endBy` fEol
fLine = fCell `sepBy` char ','
fCell = quotedCell <|> many (noneOf ",\n\r")

quotedCell = do char '"'
                content <- many quotedChar
                char '"'
                return content
quotedChar = noneOf "\"" <|> try (string "\"\"" >> return '"')
fEol = try (string "\n\r") -- try is lookahead (rewinds back if doesn't match)
     <|> try (string "\r\n")
     <|> string "\n"
     <|> string "\r"

