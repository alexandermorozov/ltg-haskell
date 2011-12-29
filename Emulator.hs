import Data.Array.MArray
import Data.Array.Diff
import Text.Printf
import Data.List
import Control.Monad.State

data    Card   = Card { cardName :: String
                      , cardN :: Int
                      , cardF :: ([Field] -> State LTG Field)
                      }

instance Eq Card where
    c == c' = cardName c == cardName c'


data    Field  = Value Int
               | Function Card [Field]

newtype Health = Health Int deriving (Show)

data    Slot   = Slot { sHealth :: Health
                      , sField :: Field
                      }

type    HBoard = DiffArray Int Slot

data    LTG = LTG { opp :: HBoard
                  , prop :: HBoard
                  , appN :: Int
                  }


-- getSlot i =

--addHealth i h = 0
--    ltg <- get

cI, cZero :: Card

cI = Card "I" 1 f
    where f [x] = return $ x

cZero = Card "zero" 0 f
    where f [] = return $ Value 0

cSucc = Card "succ" 1 f
    where f [Value x] = return $ Value (x+1)

cK = Card "K" 2 f
    where f [x, y] = return x


getPropSlot i = do
    ltg <- get
    return $ (prop ltg) ! i

putPropSlot :: Int -> Slot -> State LTG ()
putPropSlot i s = do
    ltg <- get
    put $ ltg {prop = (prop ltg) // [(i, s)]}

setPropField :: Int -> Field -> State LTG ()
setPropField i f = do
    Slot h _ <- getPropSlot i
    putPropSlot i $ Slot h f

doApp :: Int -> State LTG ()
doApp si = do -- TODO inc counter
    LTG o p n <- get
    if n >= 1000 then
        return ()
        else
            case p ! si of
                 Slot _ (Function c fs) ->
                    if cardN c /= length fs then
                            return ()
                    else do
                        f <- cardF c $ fs
                        setPropField si f

leftApp c si = do
    s <- getPropSlot si
    -- TODO: check arg n
    putPropSlot si $ Slot (sHealth s) (Function c [sField s])
    doApp si

rightApp c si = do
    Slot _ f' <- getPropSlot si
    case f' of
        Value _       -> fail "Not a function" -- TODO
        Function c' fs' -> do -- TODO check args n
            setPropField si (Function c' (fs' ++ [Function c []]))
            doApp si


--cInc = Card "inc" 1 f
--    where f [i] = addHealth i $ Health 1


createHBoard = listArray (0, 255) (repeat $ Slot (Health 10000) (Function cI []))

printHBoard :: HBoard -> IO ()
printHBoard slots = do
    let changed = filter hasChanged (assocs slots)
    mapM_ (putStrLn . format) changed
    where
        hasChanged (_, Slot (Health 10000) (Function c [])) = c /= cI
        hasChanged _ = True

        format (i, Slot (Health h) f) = printf "%d={%d,%s}" i h (formatField f)

        formatField (Value v) = show v
        formatField (Function c []) = cardName c
        formatField (Function c args) = (cardName c) ++
                "(" ++ (intercalate ", " $ map formatField args) ++ ")"


printLTG :: LTG -> IO ()
printLTG ltg = do
    putStrLn "prop:"
    printHBoard $ prop ltg
    putStrLn "opp:"
    printHBoard $ opp ltg

main = do
    let a = createHBoard
    let a' = a // [(1, Slot (Health 100) (Function cZero [])), (4, Slot (Health 10000) (Value 0))]
    let st = LTG createHBoard createHBoard 0
    --let st' = (execState (leftApp cK 1) st)
    let st' = (flip execState) st $ do
        leftApp cK 1
        leftApp cK 1
        rightApp cZero 2
        --leftApp cK 2
        --rightApp cZero 2
    printLTG st'
    putStrLn $ show (cI == cZero)
    -- printHBoard $ HBoard a'
    --putStrLn $ show b

