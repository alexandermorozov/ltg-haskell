import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Cont
import Data.Bits (shiftR, (.&.))
import Data.List (intercalate)
import Data.Tuple (swap)
import System.Environment (getArgs, getEnv)
import System.IO

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
    print $ orderToInt o
    let (s1, s2) = swapOrder o (show c, show i)
    putStrLn s1
    putStrLn s2
    hFlush stdout



type Step = (AppOrder, Card, SlotIdx)
data Action = Done | DoStep Step (LTG -> Action)
type Strategy a = StateT LTG (Cont Action) a


strategy s ltg = runCont (runStateT s ltg) (\_ -> Done)
start = step (LeftApp, cI, 0) -- first step is thrown out
step a = lift (cont $ \k -> DoStep a k) >>= put
done   = lift (cont $ \_ -> Done)



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


-- xx: fixme
putNumber :: Int -> SlotIdx -> Strategy Action
putNumber n i = do
    f <- getField Prop i
    mv <- noEffectExec $ toIntSafe f
    case f of
        Function cI [] -> step (RightApp, cZero, i)
        otherwise      ->
            case mv of
                Nothing -> step (LeftApp, cPut, i)
                Just x  -> step (LeftApp, cSucc, i) -- xx

    putNumber n i
    done
  where opsFromI 0 = 1 -- zero
        opsFromI n = 1 + msbIdx n + bitCount n -- zero, N dbls, N succs
        opsFromX n x = 1000 -- xx: fixme

msbIdx 0 = undefined
msbIdx 1 = 0
msbIdx n = 1 + msbIdx (n `shiftR` 1)

bitCount 0 = 0
bitCount n = (n .&. 1) + bitCount (n `shiftR` 1)

noEffectExec :: Strategy a -> Strategy a
noEffectExec m = do
    s <- get
    a <- m
    put s
    return a

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
    stratName <- getEnv ("BOT" ++ show i) `catch` (\_ -> return "dummy")
    let s = lookup stratName strategies
    case s of
        Just s' -> runStrategy i s'
        Nothing -> hPutStrLn stderr $ "Unknown strategy. Use one of: " ++
            intercalate ", " (map fst strategies) ++ "."
