{-# LANGUAGE OverloadedStrings #-}
module Project where

import CodeWorld

-- | Some definition of state for FSM.
data State = S0 | S1 | S2 | S3

-- | Some definition of action for FSM.
type Action = Char

-- | Transition by an action to a new state.
type Transition = (Action, State)

flatten :: [[[a]]] -> [[a]]
flatten [] = []
flatten (l:ls) = l ++ flatten ls

lang
  :: s -- ˆ Initial state.
  -> (s -> Bool) -- ˆ Is final state?
  -> (s -> [(a, s)]) -- ˆ All transitions from a given state.
  -> [[a]] -- ˆ All accepted action sequences.
lang initialState isFinal transitionsOf 
    | isFinal initialState = [[]]
    | otherwise = flatten recursiveLangs 
        where
            transitions = transitionsOf initialState
            recursiveLangs = map transitionLang transitions
            transitionLang (a, s) = map (a:) (lang s isFinal transitionsOf)

evenZeros :: [String]
evenZeros = lang initialState isFinal transitions
  where
    -- | Initial state of an FSM.
    initialState :: State
    initialState = S1

    -- | Predicate checking if a given state is final.
    isFinal :: State -> Bool
    isFinal S0 = True
    isFinal _  = False
    
    -- | All transitions for a given state.
    transitions :: State -> [Transition]
    transitions S0 = []
    transitions S1 = [('$', S0), ('0', S2)]
    transitions S2 = [('1', S3), ('0', S1)]
    transitions S3 = [('0', S1), ('1', S2)]
    
    
main :: IO ()
main = print(evenZeros)


