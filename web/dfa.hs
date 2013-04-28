{-# LANGUAGE DatatypeContexts #-}
import Data.Array

type IndirectDfa a = (Int, [IndirectState a])
 
data IndirectState a =
    IndirectState Bool [(a, Int)]

data DirectDfa a
  = DirectState Bool [(a, DirectDfa a)]

runDfa :: (Eq a) => DirectDfa a -> [a] -> Bool
runDfa (DirectState final trans) []
  = final
runDfa (DirectState final trans) (x:xs)
  = case [ s | (x',s) <- trans, x == x' ] of
        []    -> False
        (s:_) -> runDfa s xs

indirectToDirect :: IndirectDfa a -> DirectDfa a
indirectToDirect (start, states)
  = tieArray ! start
    where
        tieArray = array (0,length states - 1)
                        [ (i,direct s) | (i,s) <- zip [0..] states ]
 
        direct (IndirectState final trans)
          = DirectState final [ (x, tieArray ! s) | (x,s) <- trans ]



data (Ord l,Show l) => FaState l =
        FaState {label :: l, acceptQ :: Bool,
                 trans0:: [FaState l],
                 trans1:: [FaState l]}

type FinAu l = [FaState l]

-- Missing pattern for SelectorClosure
dom18 = [one]
     where one = FaState 1 True [one,two] []
           two = FaState 2 True [two,one] [one,two]

