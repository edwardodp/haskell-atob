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

import Types
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad (void)


interpreter :: Parser String
interpreter = undefined

instruction :: Parser Instruction
instruction = undefined

parseOnce :: Parser Once
parseOnce = undefined

parseSearch :: Parser Search
parseSearch = undefined

parseReplace :: Parser Replace
parseReplace = undefined

-- | Consume non-instruction tokens
consumeIgnore :: Parser ()
consumeIgnore = do
    many hspace1
    endLn <|> comment

-- | Consume tokens belonging to a comment
comment :: Parser ()
comment = do
    char '#'
    -- Parse any text until the end of the line (if it is compromised of ASCII characters)
    manyTill (choice [void $ oneOf "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~", hspace1, void alphaNumChar]) endLn
    pure ()

-- | Consume the end of any line
endLn :: Parser ()
endLn = choice [void eol, eof]
