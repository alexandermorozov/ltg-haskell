import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import Data.Tuple (swap)
import IO
import System.Environment (getArgs)

import LTG


{- xx: these function somewhat intersect with functions from emulator.
       Not sure if it's enough to extract them into distinct file. -}

orderToInt o = case o of LeftApp -> 1 ; RightApp -> 2
intToOrder i = case i of 1 -> LeftApp ; 2 -> RightApp

swapOrder order = case order of LeftApp -> id ; RightApp -> swap

runL :: WriterT [String] (State LTG) a -> StateT LTG IO a
runL ma = do
    ((a, _), ltg) <- liftM (runState (runWriterT ma)) get
    put ltg
    return a



readStep :: IO (AppOrder, Card, SlotIdx)
readStep = do
    order <- liftM (intToOrder . read) getLine
    (cardS, slotS) <- liftM (swapOrder order) $ liftM2 (,) getLine getLine
    return (order, cLookup cardS, read slotS)

printStep :: (AppOrder, Card, SlotIdx) -> IO ()
printStep (o, c, i) = do
    putStrLn $ show $ orderToInt o
    let (s1, s2) = swapOrder o (show c, show i)
    putStrLn s1
    putStrLn s2
    hFlush stdout


type Strategy = LTG -> (AppOrder, Card, SlotIdx)

dummyStrategy :: Strategy
dummyStrategy ltg = (LeftApp, cLookup "I", 0)


runStrategy :: Int -> Strategy -> StateT LTG IO ()
runStrategy playerN strategy = do
    forever $ case playerN of
        0 -> myTurn >> oppTurn >> runL incrementTurn
        1 -> oppTurn >> myTurn >> runL incrementTurn
    where myTurn :: StateT LTG IO ()
          myTurn = do
              st@(o, c, i) <- liftM strategy get
              liftIO $ printStep st
              runL $ applyCard o c i

          oppTurn = do
              (o, c, i) <- liftIO readStep
              runL swapPlayers
              runL $ applyCard o c i
              runL swapPlayers

main = do
    args <- getArgs
    let i = read $ head args :: Int
    runStateT (runStrategy i dummyStrategy) defaultLTG
