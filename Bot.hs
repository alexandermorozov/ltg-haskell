import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Cont
import Data.Tuple (swap)
import Data.List (intercalate)
import IO
import System.Environment (getArgs, getEnv)

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
start = step (LeftApp, cI, 0) -- first step is thrown out
step a = (lift $ cont $ \k -> DoStep a k) >>= put
done   = lift $ cont $ \_ -> Done



dummyStrategy :: Strategy Action
dummyStrategy = do
    forever $ step (LeftApp, cI, 1)
    done

decAttack :: Strategy Action
decAttack = do
    mapM_ kill [0..maxSlotIdx]
    done
    where kill i = do
              step (RightApp, cZero, 0)
              when (i > 0) $ mapM_ (\_ -> step (LeftApp, cSucc, 0)) [1..i]
              step (LeftApp, cDec, 0)
              h <- getHealth Opp (maxSlotIdx-i)
              when (h > 0) $ kill i


-- xx: draft
naivePutNumber :: Int -> SlotIdx -> Strategy Action
naivePutNumber n i = do
    step (RightApp, cZero, i)
    mapM_ (\_ -> step (RightApp, cI, i)) [1..n]
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
              oppTurn k' $ run ltg (applyCard o c i)

          oppTurn k ltg = do
              (o, c, i) <- liftIO readStep
              myTurn k $ run ltg $ do
                swapPlayers
                zombieScan
                applyCard o c i
                swapPlayers
                zombieScan

          run ltg ma = snd $ runState (runWriterT ma) ltg

strategies = 
    [ ("dummy", dummyStrategy) 
    , ("dec", decAttack)
    ]

main = do
    args <- getArgs
    let i = read $ head args :: Int
    stratName <- getEnv ("BOT"++(show i)) `catch` (\_ -> return "dummy")
    let s = lookup stratName strategies
    case s of
        Just s' -> runStrategy i s'
        Nothing -> hPutStrLn stderr $ "Unknown strategy. Use one of: " ++
            (intercalate ", " $ map fst strategies) ++ "."
