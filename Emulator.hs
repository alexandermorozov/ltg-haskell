import LTG
import System.Environment (getArgs)
import System.Process
import Control.Monad.State
import Data.Tuple

oneStep :: LTG -> IO LTG
oneStep ltg = do
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

    return $ (flip execState) ltg $ applyCard order slot card

  where parseOrder orderL =
            case read orderL of
                1 -> LeftApp
                2 -> RightApp
        getOrder = putStrLn "(1) apply card to slot, or (2) apply slot to card?" >>
                       liftM parseOrder getLine
        getCard = putStrLn "card name?" >> liftM cLookup getLine
        getSlot = putStrLn "slot no?" >> liftM read getLine


runOnly :: IO LTG
runOnly = do
    helper defaultLTG
    where helper ltg = oneStep ltg >>= helper

runAlt :: IO LTG
runAlt = do
    helper defaultLTG
    where helper ltg =
            oneStep ltg >>=
            oneStep . swapPlayers >>=
            helper . incrementTurn . swapPlayers

runMatch [prog0, prog1] = do
    (Just s0, Just o0, _, _) <- cProc prog0 "0"
    (Just s1, Just o1, _, _) <- cProc prog1 "1"
    helper defaultLTG
    where
        cProc prog i = createProcess
            (proc prog [i]) { std_in = CreatePipe, std_out = CreatePipe}

        helper ltg = return ltg


main = do
    args <- getArgs
    when (length args == 0) $ fail "Usage: ./xx only|alt|match [prog1] [prog2]"
    case head args of
        "only"  -> runOnly
        "alt"   -> runAlt
        "match" -> runMatch $ tail args
        _       -> fail "Unknown command"

    return 0

