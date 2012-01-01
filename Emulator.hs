import LTG
import System.Environment (getArgs)
import Control.Monad.State


oneStep :: LTG -> IO LTG
oneStep ltg = do
    putStrLn $ "###### turn " ++ show(ltgTurn ltg)
    putStrLn $ "*** player " ++ show(ltgPlayer ltg) ++ "'s turn, with slots:"
    printHBoard Prop ltg
    putStrLn "(slots {10000,I} are omitted)"

    putStrLn "(1) apply card to slot, or (2) apply slot to card?"
    order <- liftM parseOrder getLine

    putStrLn "card name?"
    card <- liftM cLookup getLine

    putStrLn "slot no?"
    slot <- liftM read getLine :: IO Int

    putStrLn $ case order of
        LeftApp ->  "player 0 applied card " ++ (show card) ++ " to slot " ++ (show slot)
        RightApp -> "player 0 applied slot " ++ (show slot) ++ " to card " ++ (show card)

    return $ (flip execState) ltg $ applyCard order slot card

  where parseOrder orderL =
            case read orderL of
                1 -> LeftApp
                2 -> RightApp


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

main = do
    args <- getArgs
    when (length args == 0) $ fail "Usage: ./xx only|alt|match [prog1] [prog2]"
    case head args of
        "only"  -> runOnly
        "alt"   -> runAlt
        "match" -> fail "Not implemented"
        _       -> fail "Unknown command"

    return 0

