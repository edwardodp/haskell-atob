module Parsers
    ( interpreter
    , instruction
    , parseOnce
    , parseSearch
    , parseReplace
    , consumeIgnore
    , comment
    , endLn
    )
where

import Control.Monad (void)
import Control.Monad.Combinators
import Data.Char (isAlphaNum)
import Program
import Text.Megaparsec
import Text.Megaparsec.Char
import Types


interpreter :: String -> Parser String
interpreter inputStr = do
    input <- strLegal inputStr
    manyConsumeIgnores -- Ignore leading whitespace/comments
    instructions <- many instruction -- Any instruction (at this point) will have to be 'valid'
    pure $ processOutput instructions input
 
instruction :: Parser Instruction
instruction = do
    once <- parseOnce
    search <- parseSearch
    char '='
    replace <- parseReplace
    consumeIgnore >> manyConsumeIgnores -- Equivalent to someConsumeIgnores

    pure $ Instruction once search replace

parseOnce :: Parser Once
parseOnce = do
    prefix <- option False $ True <$ string "(once)"
    pure $ isPrefixed prefix

{-
Returns the appropitate Once type depending on if the search is
prefixed by (once).
-}
isPrefixed :: Bool -> Once
isPrefixed prefixed
    | prefixed      = Once True
    | otherwise     = Always

parseSearch :: Parser Search
parseSearch = do
        constructor <- Start <$ string "(start)" <|> End <$ string "(end)" <|> pure First
        search <- manyTill parseLegalChar $ lookAhead (char '=')
        pure $ constructor search

parseReplace :: Parser Replace
parseReplace = do
    constructor <- Enqueue <$ string "(start)" <|> Push <$ string "(end)" <|> Return <$ string "(return)" <|> pure Replace
    replace <- manyTill parseLegalChar $ lookAhead consumeIgnore
    pure $ constructor replace

-- Does not parse spaces or the following reserved symbols ()#=
parseLegalChar :: Parser Char
parseLegalChar = satisfy legalChar

-- Lift the value such that if it is illegal, we lift Left on the Parser
strLegal :: String -> Parser String
strLegal input
    | all legalChar input    = pure input
    | otherwise         = error "Input string is non-valid. It must not contain any whitespace or one of the following reserved characters ()#="

manyConsumeIgnores :: Parser ()
manyConsumeIgnores = do
    -- True if eof reached or more code reached, False if still parsing comments/whitespace
    finishParsing <- option True consumeIgnore
    case finishParsing of
        False -> manyConsumeIgnores
        True  -> pure ()

-- | Consume non-instruction tokens
consumeIgnore :: Parser Bool
consumeIgnore = do
    many hspace1
    isEof <- endLn <|> False <$ comment
    pure isEof

-- | Consume tokens belonging to a comment
comment :: Parser ()
comment = do
    char '#'
    -- Parse any text until the end of the line (if it is compromised of ASCII characters)
    manyTill (choice [void $ oneOf "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~", hspace1, void alphaNumChar]) endLn
    pure ()

-- | Consume the end of any line
endLn :: Parser Bool
endLn = choice [False <$ eol, True <$ eof]

legalChar :: Char -> Bool
legalChar c = c `elem` legalSymbols || isAlphaNum c

legalSymbols :: String
legalSymbols = "!\"$%&'*+,-./:;<>?@[\\]^_`{|}~"
