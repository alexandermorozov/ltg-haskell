import IO
import System.Environment (getArgs)
import System.Process
import Control.Monad.State
import Control.Monad.Error
import Data.Tuple (swap)
import Data.Maybe

import LTG

{- TODO: too many ltg, ltg', ltg''. Maybe I need StateT, WriterT  or something?
-}

oneStep :: Handle -> Maybe Handle -> LTG -> IO LTG
oneStep hIn mOut ltg = do
    putStrLn $ "*** player " ++ show(ltgPlayer ltg) ++ "'s turn, with slots:"
    printHBoard Prop ltg
    putStrLn "(slots {10000,I} are omitted)"

    let (ltg', msgs') = zombieScan ltg
    mapM_ putStrLn msgs'

    order <- getOrder
    -- xx: too complex? any simpler way?
    (card, slot) <- case order of
            LeftApp -> liftM2 (,) getCard getSlot
            RightApp -> liftM swap $ liftM2 (,) getSlot getCard

    putStrLn $ showChoice (ltgPlayer ltg') order card slot

    case mOut of
        Just h -> (hPutStr h $ showChoice4Bot order card slot) >> hFlush h

    let (ltg'', msgs'') = applyCard order slot card ltg'
    mapM_ putStrLn msgs''
    --when (isJust err) $ putStrLn $ "Exception: " ++ fromJust err
    return ltg''

  where getOrder = do
            putStrLn "(1) apply card to slot, or (2) apply slot to card?"
            liftM (intToOrder . read) (hGetLine hIn)
        getCard = putStrLn "card name?" >> liftM cLookup (hGetLine hIn)
        getSlot = putStrLn "slot no?" >> liftM read (hGetLine hIn)

        onOrder order a b = case order of LeftApp -> a; RightApp -> b

        showChoice pn order card slot =
            let p1 = ["player ", show(pn), " applied "]
                x  = onOrder order id swap ("card " ++ show card, "slot " ++ show slot)
            in concat $ p1 ++ [fst x, " to ", snd x]

        showChoice4Bot order card slot =
            let p1 = show $ orderToInt order
                p2 = onOrder order id reverse $ [show card, show slot]
            in concat $ map (++"\n") (p1:p2)

printTurn ltg = putStrLn $ "###### turn " ++ show(ltgTurn ltg)

orderToInt o = case o of LeftApp -> 1 ; RightApp -> 2
intToOrder i = case i of 1 -> LeftApp ; 2 -> RightApp

simpleStep :: LTG -> IO LTG
simpleStep ltg = printTurn ltg >> oneStep stdin Nothing ltg

runOnly :: IO LTG
runOnly = do
    helper defaultLTG
    where helper ltg = simpleStep ltg >>= helper

runAlt :: IO LTG
runAlt = do
    helper defaultLTG
    where helper ltg =
            simpleStep ltg >>=
            simpleStep . swapPlayers >>=
            helper . incrementTurn . swapPlayers

runMatch [prog0, prog1] = do
    (Just in0, Just out0, _, _) <- cProc prog0 "0"
    (Just in1, Just out1, _, _) <- cProc prog1 "1"
    helper in0 out0 in1 out1 defaultLTG
    where
        cProc prog i = createProcess (proc prog [i]) {std_in = CreatePipe,
                                                      std_out = CreatePipe}

        helper :: Handle -> Handle -> Handle -> Handle -> LTG -> IO LTG
        helper in0 out0 in1 out1 ltg = do
            printTurn ltg
            ltg1 <- oneStep out0 (Just in1) ltg
            -- putStrLn $ show $ countAlive ltg1
            ltg2 <- oneStep out1 (Just in0) (swapPlayers ltg1)
            case ltgTurn ltg2 of
                100000 -> return ltg2
                _      -> helper in0 out0 in1 out1 . incrementTurn . swapPlayers $ ltg2

        --printActions h order card slot = do

main = do
    args <- getArgs
    when (length args == 0) $ fail "Usage: ./xx only|alt|match [prog1] [prog2]"
    case head args of
        "only"  -> runOnly
        "alt"   -> runAlt
        "match" -> runMatch $ tail args
        _       -> fail "Unknown command"

    return 0

