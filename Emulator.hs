import IO
import System.Environment (getArgs)
import System.Process
import Control.Monad.State
import Data.Tuple (swap)

import LTG

oneStep :: Handle -> Maybe Handle -> LTG -> IO LTG
oneStep hIn mOut ltg = do
    putStrLn $ "###### turn " ++ show(ltgTurn ltg)
    putStrLn $ "*** player " ++ show(ltgPlayer ltg) ++ "'s turn, with slots:"
    printHBoard Prop ltg
    putStrLn "(slots {10000,I} are omitted)"

    order <- getOrder
    -- xx: too complex? any simpler way?
    (card, slot) <- case order of
            LeftApp -> liftM2 (,) getCard getSlot
            RightApp -> liftM swap $ liftM2 (,) getSlot getCard

    putStrLn $ case order of
        LeftApp ->  "player 0 applied card " ++ (show card) ++ " to slot " ++ (show slot)
        RightApp -> "player 0 applied slot " ++ (show slot) ++ " to card " ++ (show card)

    case mOut of
        Just h -> (hPutStr h $ prepareOutput order card slot) >> hFlush h

    let ltg' = (flip execState) ltg $ applyCard order slot card
    return ltg'

  where parseOrder orderL =
            case read orderL of
                1 -> LeftApp
                2 -> RightApp
        getOrder = putStrLn "(1) apply card to slot, or (2) apply slot to card?" >>
                       liftM parseOrder (hGetLine hIn)
        getCard = putStrLn "card name?" >> liftM cLookup (hGetLine hIn)
        getSlot = putStrLn "slot no?" >> liftM read (hGetLine hIn)

        prepareOutput order card slot =
            let p1 = show $ orderToInt order
                p2 = case order of
                        LeftApp  -> [show card, show slot]
                        RightApp -> [show slot, show card]
            in concat $ map (++"\n") (p1:p2)

orderToInt o = case o of LeftApp -> 1 ; RightApp -> 2
intToOrder i = case i of 1 -> LeftApp ; 2 -> RightApp

simpleStep :: LTG -> IO LTG
simpleStep = oneStep stdin Nothing

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
            ltg1 <- oneStep out0 (Just in1) ltg
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

