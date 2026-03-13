{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.Utils.StateMachine where

import Kernel.Prelude

class (Eq s, Show s) => StateMachine s where
  validTransitions :: s -> [s]
  isTerminal :: s -> Bool
  initialState :: s

data TransitionResult s
  = TransitionOk s
  | InvalidTransition s s [s]
  deriving (Eq, Show)

transition :: StateMachine s => s -> s -> TransitionResult s
transition from to
  | to `elem` validTransitions from = TransitionOk to
  | otherwise = InvalidTransition from to (validTransitions from)

validateSequence :: StateMachine s => [s] -> [TransitionResult s]
validateSequence [] = []
validateSequence [_] = []
validateSequence (x : y : rest) =
  let result = transition x y
   in result : case result of
        TransitionOk _ -> validateSequence (y : rest)
        InvalidTransition {} -> []
