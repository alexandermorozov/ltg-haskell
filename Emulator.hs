import IO
import System.Environment (getArgs)
import System.Process
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Error
import Data.Tuple (swap)
import Data.Maybe

import LTG


runL :: WriterT [String] (State LTG) a -> StateT LTG IO a
runL ma = do
    ((a, msgs), ltg) <- liftM (runState (runWriterT ma)) get
    put ltg
    mapM_ (liftIO . putStrLn) msgs
    return a

-- xx: fix evil liftIO
oneStep :: Handle -> Maybe Handle -> StateT LTG IO ()
oneStep hIn mOut = do
    playerNo <- liftM ltgPlayer get
    liftIO . putStrLn $ "*** player " ++ show(playerNo) ++ "'s turn, with slots:"
    runL $ printHBoard Prop
    liftIO . putStrLn $ "(slots {10000,I} are omitted)"

    runL zombieScan

    order <- getOrder
    -- xx: too complex? any simpler way?
    (card, slot) <- case order of
            LeftApp -> liftM2 (,) getCard getSlot
            RightApp -> liftM swap $ liftM2 (,) getSlot getCard

    (liftIO . putStrLn) $ showChoice playerNo order card slot

    case mOut of
        Just h -> do
            liftIO . hPutStr h $ showChoice4Bot order card slot
            liftIO $ hFlush h

    runL $ applyCard order card slot

  where getOrder = do
            (liftIO . putStrLn) "(1) apply card to slot, or (2) apply slot to card?"
            liftM (intToOrder . read) (liftIO $ hGetLine hIn)
        getCard = do
            (liftIO . putStrLn) "card name?"
            liftM cLookup (liftIO $ hGetLine hIn)
        getSlot = do
            (liftIO . putStrLn) "slot no?"
            liftM read (liftIO $ hGetLine hIn)

        onOrder order a b = case order of LeftApp -> a; RightApp -> b

        showChoice pn order card slot =
            let p1 = ["player ", show(pn), " applied "]
                x  = onOrder order id swap ("card " ++ show card, "slot " ++ show slot)
            in concat $ p1 ++ [fst x, " to ", snd x]

        showChoice4Bot order card slot =
            let p1 = show $ orderToInt order
                p2 = onOrder order id reverse $ [show card, show slot]
            in concat $ map (++"\n") (p1:p2)

printTurn :: StateT LTG IO ()
printTurn = get >>= \ltg -> liftIO $ putStrLn $ "###### turn " ++ show(ltgTurn ltg)

orderToInt o = case o of LeftApp -> 1 ; RightApp -> 2
intToOrder i = case i of 1 -> LeftApp ; 2 -> RightApp

simpleStep :: StateT LTG IO ()
simpleStep = printTurn >> oneStep stdin Nothing

runOnly :: IO LTG
runOnly = execStateT (forever simpleStep) defaultLTG

runAlt :: IO LTG
runAlt = execStateT (forever helper) defaultLTG
    where helper = do
            printTurn
            simpleStep ; runL swapPlayers
            simpleStep ; runL swapPlayers
            runL incrementTurn

runMatch :: [String] -> IO LTG
runMatch [prog0, prog1] = do
    (Just in0, Just out0, _, _) <- cProc prog0 "0"
    (Just in1, Just out1, _, _) <- cProc prog1 "1"
    execStateT (helper in0 out0 in1 out1) defaultLTG
    where
        cProc prog i = createProcess (proc prog [i]) {std_in = CreatePipe,
                                                      std_out = CreatePipe}

        -- xx: remove staircasing. Cont? Either?
        helper :: Handle -> Handle -> Handle -> Handle -> StateT LTG IO LTG
        helper in0 out0 in1 out1 = do
            printTurn
            oneStep out0 (Just in1) ; runL swapPlayers
            eMsg <- checkEnd
            case eMsg of
                Just m -> (liftIO . putStrLn) m >> get
                Nothing -> do
                    oneStep out1 (Just in0) ; runL swapPlayers
                    eMsg <- checkEnd
                    case eMsg of
                        Just m -> (liftIO . putStrLn) m >> get
                        Nothing -> runL incrementTurn >> helper in0 out0 in1 out1

        checkEnd = do
            (l0, l1) <- runL countAlive
            turn <- liftM ltgTurn get
            playerI <- liftM ltgPlayer get
            let msg = compose l0 l1 turn
            if (l0 == 0 || l1 == 0)
                then return (Just msg)
                else if turn == 100000 && playerI == 1
                    then return (Just msg)
                    else return Nothing
            where compose l0 l1 turn =
                      let msg2 = " by " ++ show l0 ++ ":" ++ show l1 ++
                                 " after turn " ++ show turn
                      in case (l0 == l1, l0 > l1) of
                          (True, _) -> "!! draw" ++ msg2
                          (_, True) -> "!! player 0 wins" ++ msg2
                          (_, _   ) -> "!! player 1 wins" ++ msg2

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

