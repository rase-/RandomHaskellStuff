module MarkovChain
(
) where

import Control.Monad.State
import qualified Data.Map as M

type MarkovState = Int
type Count = Int

data MarkovTransition = MarkovTransition { getState :: MarkovState, getCount :: Count }
data TransitionMatrix = TransitionMatrix (M.Map Int [MarkovTransition])

transition :: MarkovState -> TransitionMatrix -> MarkovState
transition s (TransitionMatrix m) = undefined
  where candidates = case M.lookup s m of Just x -> getState $ foldl (\m@(MarkovTransition ms mc) el@(MarkovTransition s c) -> if c > mc then el else m)
                                                                     (MarkovTransition (-1) (-1))
                                                                     x
                                          Nothing -> fst modeState
        modeState = M.fold (\max@(ms,mc) el@(s,c) -> if c > mc then el else max) ((-1), (-1)) stateCounts
        stateCounts = M.mapWithKey (\k v -> (k, (sum . map getCount) v)) m

