module Types 
    ( Parser
    , Once (..)
    , Search (..)
    , Replace (..)
    , Instruction (Instruction)
    )
where

import Text.Megaparsec
import Data.Void (Void)


type Parser = Parsec Void String

{- |
 A @(once)@ at the beggining of the line will initially set this to @Once True@;
 after it has run once, it will become @Once False@.
 Otherwise this type will be set to @Always@ to always run.
-}
data Once = Always | Once Bool
    deriving (Eq, Show)

{- |
 @(start)string1@ parses to @Start string1@
 @string1@ parses to @First string1@
 @(end)string1@ parses to @End string1@
-}
data Search = Start String | First String | End String
    deriving (Eq, Ord, Show)

{- |
@=(start)string2@ parses to @Enqueue string2@
@=string2@ parses to @Replace string2@
@=(end)string2@ parses to @Push string2@
@=(return)string2@ parses to @Return string2@
-}
data Replace = Enqueue String | Replace String | Push String | Return String
    deriving (Eq, Ord, Show)

{- |
 *run* stores the once status of the expression
*a* stores the left string that will be replaced
*b* stores the operation to perform with the right string, as well as the string that will replace
-}
data Instruction = Instruction
    { run :: Once
    , a :: Search
    , b :: Replace
    } deriving (Show)
