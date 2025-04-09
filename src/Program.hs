module Program
    ( processOutput
    , shouldRun
    , searchMatch
    , listIndex
) where

import Data.List (tails, elemIndex)
import Data.Maybe (isNothing)
import Types


-- As a prereq, the input string is already checked for legality and all instructions are valid
{- | 
Loop through instructions in declarative order until an instruction matching the search string against the input string
Apply the instruction to the input string and then loop back to beggining of the instruction list with the modified input string
Repeat until no instruction has a match with the search string at which point the modified input string will be returned
If OS memory limit is exceeded, the program will also terminate
-}
processOutput :: [Instruction] -> String -> String
processOutput instructions input = undefined

{- |
Determines if an instruction can be executed. If it can, and it is of type Once, then it will update the instruction to reflect it shall not run again
Just => Execute existing instruction and update instruction with newly provided instruction
Nothing => Don't execute instuction
The output instruciton is the updated instruction
-}
shouldRun :: Instruction -> String -> Maybe Instruction
shouldRun instruction input
    | isNothing (snd $ searchMatch instruction input)  = Nothing
    | run instruction == Always                        = Just instruction
    | run instruction == Once True                     = Just $ instruction {run = Once False}
    | otherwise                                        = Nothing

-- Return the updated string without the search string as well as the integer of where it was located
searchMatch :: Instruction -> String -> (String, Maybe Int)
searchMatch instruction input = case listIndex input (getSearch $ search instruction) of
    Nothing -> (input, Nothing)
    Just searchInd -> case search instruction of 
        Start substr -> if substr == take (length substr) input then (drop (length substr) input, Just 0) else (input, Nothing)
        End substr -> if substr == drop (length input - length substr) input then (take (length input - length substr) input, Just $ length input - length substr) else (input, Nothing)
        First substr -> (take searchInd input ++ drop (length substr + searchInd) input, Just searchInd)

listIndex :: Eq a => [a] -> [a] -> Maybe Int
listIndex xs subx = elemIndex subx $ map (take $ length subx) $ tails xs
