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

newtype Health = Health Int deriving (Show, Eq, Ord) -- xx: delete?
data    Slot   = Slot { sHealth :: Health, sField :: Field }
type    HBoard = DiffArray Int Slot

data    LTG = LTG { opp :: HBoard
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
          s@(Slot _ f) <- getPropSlot i'
          if isAlive s
              then return f
              else fail "Slot is dead"

cPut = Card "put" 1 $ \[_] -> return $ Function cI []

cK = Card "K" 2 $ \[x,y] -> return x

cS = Card "S" 3 $ \[f,g,x] -> do
          h <- apply f x
          y <- apply g x
          apply h y


getPropSlot i = do
    ltg <- get
    return $ (prop ltg) ! i

putPropSlot :: Int -> Slot -> State LTG ()
putPropSlot i s = do
    ltg <- get
    put $ ltg {prop = (prop ltg) // [(i, s)]}

putPropField :: Int -> Field -> State LTG ()
putPropField i f = do
    s <- getPropSlot i
    putPropSlot i s {sField = f}

isAlive :: Slot -> Bool
isAlive s = sHealth s > Health 0

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

incAppCounter :: State LTG ()
incAppCounter = do
    ltg@(LTG _ _ n) <- get
    when (n == 1000) $ fail "Recursion depth exceeded"
    put $ ltg {appN = n + 1}

resetAppCounter :: State LTG ()
resetAppCounter = do
    ltg <- get
    put $ ltg {appN = 0}



-- xx: merge those functions?
-- apply card to slot
leftApp si c = do
    resetAppCounter
    Slot h f <- getPropSlot si
    when (h <= Health 0) $ fail "Slot is dead"
    f' <- apply (Function c []) f
    putPropField si f'

-- apply slot to card
rightApp si c = do
    resetAppCounter
    Slot h f <- getPropSlot si
    when (h <= Health 0) $ fail "Slot is dead"
    f' <- apply f (Function c [])
    putPropField si f'



createHBoard = listArray (0, 25) (repeat $ Slot (Health 10000) (Function cI []))

printHBoard :: HBoard -> IO ()
printHBoard slots = do
    let changed = filter hasChanged (assocs slots)
    mapM_ (putStrLn . format) changed
    where
        hasChanged (_, Slot (Health 10000) (Function c [])) = c /= cI
        hasChanged _ = True

        format (i, Slot (Health h) f) = printf "%d={%d,%s}" i h (show f)



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
        rightApp 1 cK
        leftApp 1 cK
        rightApp 4 cK
        rightApp 2 cZero
        leftApp 2 cSucc
        leftApp 2 cSucc
        leftApp 2 cDbl
        leftApp 2 cGet
        leftApp 2 cPut
        --leftApp cK 2
        --rightApp cZero 2
    printLTG st'
    -- printHBoard $ HBoard a'
    --putStrLn $ show b

