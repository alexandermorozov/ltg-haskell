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

data    Field  = Value Int | Function Card [Field]
newtype Health = Health Int deriving (Show, Eq, Ord) -- xx: delete?
data    Slot   = Slot { sHealth :: Health, sField :: Field }
type    HBoard = DiffArray Int Slot

data    LTG = LTG { opp :: HBoard
                  , prop :: HBoard
                  , appN :: Int
                  }


-- xx: replace 'fail' with a monad transformer

cI, cZero :: Card

cI = Card "I" 1 f
    where f [x] = return $ x

cZero = Card "zero" 0 f
    where f [] = return $ Value 0

cSucc = Card "succ" 1 f
    where f [x] = do
            x' <- toInt x
            return $ Value (x' + 1)

cK = Card "K" 2 f
    where f [x, y] = return x



toInt :: Field -> State LTG Int
toInt f = do
    f' <- forceApp f
    case f' of
        Value i -> return i
        _       -> fail "Not an integer"

getPropSlot i = do
    ltg <- get
    return $ (prop ltg) ! i

putPropSlot :: Int -> Slot -> State LTG ()
putPropSlot i s = do
    ltg <- get
    put $ ltg {prop = (prop ltg) // [(i, s)]}

putPropField :: Int -> Field -> State LTG ()
putPropField i f = do
    Slot h _ <- getPropSlot i
    putPropSlot i $ Slot h f

isAlive :: Slot -> Bool
isAlive s = sHealth s > Health 0

forceApp :: Field -> State LTG Field
forceApp f = do
    case f of
        Value i -> return f
        Function c args -> do
            if cardN c /= length args
                then return f
                else incAppCount >> (cardF c) args
    where incAppCount = do
            ltg@(LTG _ _ n) <- get
            case n of -- xx: is it the right place?
                1000 -> fail "Recursion depth exceeded"
            put $ ltg {appN = n + 1}


-- apply card to slot
leftApp c si = do
    -- xx: check card arg n
    --     check if slot is dead
    Slot h f <- getPropSlot si
    f' <- forceApp $ Function c [f]
    putPropField si f'

-- apply slot to card
rightApp c si = do
    -- xx: check slot arg n
    --     check if slot is dead
    Slot h f <- getPropSlot si
    case f of
        Value _         -> fail "Not a function"
        Function c' fs' -> -- xx check args n
            forceApp (Function c' (fs' ++ [Function c []])) >>= putPropField si




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
        rightApp cK 1
        leftApp cK 1
        rightApp cZero 2
        leftApp cSucc 2
        leftApp cSucc 2
        rightApp cZero 3
        --leftApp cK 2
        --rightApp cZero 2
    printLTG st'
    putStrLn $ show (cI == cZero)
    -- printHBoard $ HBoard a'
    --putStrLn $ show b

