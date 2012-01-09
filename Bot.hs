import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Cont
import Data.Tuple (swap)
import IO
import System.Environment (getArgs)

import LTG


{- xx: these function somewhat intersect with functions from emulator.
       Not sure if it's enough to extract them into distinct file. -}

orderToInt o = case o of LeftApp -> 1 ; RightApp -> 2
intToOrder i = case i of 1 -> LeftApp ; 2 -> RightApp

swapOrder order = case order of LeftApp -> id ; RightApp -> swap



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



type Step = (AppOrder, Card, SlotIdx)
data Action = Done | DoStep Step (LTG -> Action)
type Strategy a = StateT LTG (Cont Action) a


strategy s ltg = runCont (runStateT s ltg) (\_ -> Done)
start = step (LeftApp, cLookup "I", 0) -- first step is thrown out
step a = (lift $ cont $ \k -> DoStep a k) >>= put
done   = lift $ cont $ \_ -> Done


dummyStrategy :: Strategy Action
dummyStrategy = do
    forever $ step (LeftApp, cLookup "I", 1)
    done

decAttack :: Strategy Action
decAttack = do
    forever $ do
        step (RightApp, cLookup "zero", 0)
        step (LeftApp, cLookup "dec", 0)
    done

runStrategy :: Int -> Strategy Action -> IO ()
runStrategy playerN s =
    let (DoStep _ a) = strategy (start >> s) defaultLTG
    in case playerN of
         0 -> myTurn a defaultLTG
         1 -> oppTurn a defaultLTG
    where myTurn k ltg = do
              let (DoStep (o, c, i) k') = k ltg
              printStep (o, c, i)
              oppTurn k' $ run (applyCard o c i) ltg

          oppTurn k ltg = do
              (o, c, i) <- liftIO readStep
              myTurn k $ run (swapPlayers >> applyCard o c i >> swapPlayers) ltg

          run ma ltg = snd $ runState (runWriterT ma) ltg

main = do
    args <- getArgs
    let i = read $ head args :: Int
    --runStrategy i dummyStrategy
    runStrategy i decAttack
