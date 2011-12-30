import Data.Array.MArray
import Data.Array.Diff
import Text.Printf
import Data.List
import Control.Monad.State
import Debug.Trace

data    Card   = Card { cardName :: String
                      , cardN :: Int
                      , cardF :: ([Field] -> State LTG Field)
                      }

data    Field  = Value Int | Function Card [Field]

type Health   = Int
type SlotIdx  = Int
data Slot     = Slot { sHealth :: Health, sField :: Field }
type HBoard   = DiffArray SlotIdx Slot
data AppOrder = LeftApp | RightApp
data Player   = Opp | Prop deriving (Eq)
data LTG      = LTG { opp :: HBoard
                    , prop :: HBoard
                    , appN :: Int
                    }

instance Eq Card where
    c == c' = cardName c == cardName c'

instance Show Field where
    show (Value v) = show v
    show (Function c []) = cardName c
    show (Function c args) = (cardName c) ++
                "(" ++ (intercalate ", " $ map show args) ++ ")"


-- xx: replace 'fail' with a monad transformer
-- xx: 'case' syntax and indentation. Replace with if/then?

cI = Card "I" 1 $ \[x] ->
          return $ x

cZero = Card "zero" 0 $ \[] ->
          return $ Value 0

cSucc = Card "succ" 1 $ \[x] -> do
          x' <- toInt x
          return $ Value $ min 65636 (x' + 1)

cDbl = Card "dbl" 1 $ \[x] -> do
          x' <- toInt x
          return $ Value $ min 65636 (2 * x')

cGet = Card "get" 1 $ \[i] -> do
          i' <- toInt i
          s@(Slot _ f) <- getSlot Prop i'
          if isAlive s
              then return f
              else fail "Slot is dead"

cPut = Card "put" 1 $ \[_] -> return $ Function cI []

cS = Card "S" 3 $ \[f,g,x] -> do
          h <- apply f x
          y <- apply g x
          apply h y

cK = Card "K" 2 $ \[x,y] -> return x

cInc = Card "inc" 1 $ \[i] -> do
          i' <- toSlotNumber i
          s@(Slot h _) <- getSlot Prop i'
          when (h > 0 && h < 65535) $ putSlot Prop i' s {sHealth = (h+1)}
          return $ Function cI []

cDec = Card "dec" 1 $ \[i] -> do
          i' <- liftM (\a -> 255 - a) $ toSlotNumber i
          s@(Slot h _) <- getSlot Opp i'
          when (h > 0) $ putSlot Opp i' s {sHealth = (h-1)}
          return $ Function cI []

getSlot :: Player -> SlotIdx -> State LTG Slot
getSlot p i = do
    let selector = if (p == Prop) then prop else opp
    ltg <- get
    return $ (selector ltg) ! i


putSlot :: Player -> SlotIdx -> Slot -> State LTG ()
putSlot p i s = do
    ltg <- get
    put $ if (p == Prop)
        then ltg {prop = (prop ltg) // [(i, s)]}
        else ltg {opp  = (opp  ltg) // [(i, s)]}

putField :: Player -> Int -> Field -> State LTG ()
putField p i f = do
    s <- getSlot p i
    putSlot p i s {sField = f}

modifyHealth :: Player -> SlotIdx -> Health -> State LTG ()
modifyHealth p i dh = do
    (Slot h f) <- getSlot p i
    let new = min 65535 $ max 0 $ h + dh
    putSlot p i (Slot new f)

isAlive :: Slot -> Bool
isAlive s = sHealth s > 0


apply :: Field -> Field -> State LTG Field
apply a b =
    case a of
        Value i -> fail "Not a function"
        Function cA argsA ->
            case cardN cA - length argsA of
                0 -> fail "Too many arguments"
                1 -> incAppCounter >> (cardF cA) (argsA ++ [b])
                _ -> return $ Function cA $ argsA ++ [b]

toInt :: Field -> State LTG Int
toInt f =
    case f of
        Value x -> return x
        Function c args -> do
            when (cardN c /= length args) $ fail "Cannot coerce to integer"
            incAppCounter
            f' <- (cardF c) args
            toInt f'

toSlotNumber :: Field -> State LTG SlotIdx
toSlotNumber f = do
    i <- toInt f
    when (i > 255 || i < 0) $ fail "Invalid slot number"
    return i

incAppCounter :: State LTG ()
incAppCounter = do
    ltg@(LTG _ _ n) <- get
    when (n == 1000) $ fail "Recursion depth exceeded"
    put $ ltg {appN = n + 1}

resetAppCounter :: State LTG ()
resetAppCounter = do
    ltg <- get
    put $ ltg {appN = 0}

reverseBoard :: State LTG ()
reverseBoard = do
    ltg <- get
    put $ ltg {prop = opp ltg, opp = prop ltg}

applyCard order i c = do
    resetAppCounter
    Slot h f <- getSlot Prop i
    when (h <= 0) $ fail "Slot is dead"
    f' <- case order of
            LeftApp  -> apply (Function c []) f
            RightApp -> apply f (Function c [])
    putField Prop i f'

-- convinience
rightApp = applyCard RightApp
leftApp  = applyCard LeftApp


createHBoard = listArray (0, 255) (repeat $ Slot 10000 (Function cI []))

printHBoard :: HBoard -> IO ()
printHBoard slots = do
    let changed = filter hasChanged (assocs slots)
    mapM_ (putStrLn . format) changed
    where
        hasChanged (_, Slot 10000 (Function c [])) = c /= cI
        hasChanged _ = True

        format (i, Slot h f) = printf "%d={%d,%s}" i h (show f)



printLTG :: LTG -> IO ()
printLTG ltg = do
    putStrLn "prop:"
    printHBoard $ prop ltg
    putStrLn "opp:"
    printHBoard $ opp ltg

main = do
    let st = LTG createHBoard createHBoard 0
    let st' = (flip execState) st $ do
        rightApp 1 cK
        rightApp 2 cZero
        leftApp  2 cDec
        --leftApp cK 2
        --rightApp cZero 2
    printLTG st'
    -- printHBoard $ HBoard a'
    --putStrLn $ show b

