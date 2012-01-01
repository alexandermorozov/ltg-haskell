module LTG
    (
      AppOrder (..)
    , LTG (ltgAppN, ltgTurn, ltgPlayer)
    , Card
    , Player (..)
    , cLookup
    , defaultLTG
    , applyCard
    , swapPlayers
    , incrementTurn
    , printHBoard
    ) where

{-
TODO:
   * custom monad. As far as I understand, there is no way to combine State
        monad with another transformer to get desired termination rules.
   * zombie mode / scan

Not sure:
   * It could be nice to split this module, but then I'll have to export too
        much functions.
-}

import Data.Array.Diff
import Text.Printf
import Data.List (intercalate)
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
data LTG      = LTG { ltgOpp :: HBoard
                    , ltgProp :: HBoard
                    , ltgAppN :: Int -- xx: not sure if it should be here
                    , ltgTurn :: Int
                    , ltgPlayer :: Int -- 0 or 1
                    }

instance Eq Card where
    c == c' = cardName c == cardName c'

instance Show Card where
    show c = cardName c

instance Show Field where
    show (Value v) = show v
    show (Function c []) = cardName c
    show (Function c args) = (cardName c) ++
                "(" ++ (intercalate ", " $ map show args) ++ ")"



------------------------------------------------------- Cards

cI = Card "I" 1 $ \[x] ->
          return x

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

cPut = Card "put" 1 $ \[_] -> returnI

cS = Card "S" 3 $ \[f,g,x] -> do
          h <- apply f x
          y <- apply g x
          apply h y

cK = Card "K" 2 $ \[x,y] -> return x

cInc = Card "inc" 1 $ \[i] -> do
          toSlotNumber i >>= modifyHealth Prop 1
          returnI

cDec = Card "dec" 1 $ \[i] -> do
          toSlotNumber i >>= modifyHealth Opp (-1)
          returnI

cAttack = Card "attack" 3 $ \[i,j,n] -> do
          i' <- toSlotNumber i
          n' <- toInt n
          ph <- getHealth Prop i'
          when (n' > ph) $ fail "Not enough vitality"
          modifyHealth Prop i' (-n')

          j' <- toSlotNumber j
          modifyHealth Opp (255-j') $ -n'*9 `quot` 10
          returnI

cHelp = Card "help" 3 $ \[i,j,n] -> do
          i' <- toSlotNumber i
          n' <- toInt n
          ph <- getHealth Prop i'
          when (n' > ph) $ fail "Not enough vitality"
          modifyHealth Prop i' (-n')

          j' <- toSlotNumber j
          modifyHealth Prop j' $ n'*11 `quot` 10
          returnI

cCopy = Card "copy" 1 $ \[i] -> do
          i' <- toSlotNumber i
          getField Opp i'

cRevive = Card "revive" 1 $ \[i] -> do
          i' <- toSlotNumber i
          ph <- getHealth Prop i'
          when (ph <= 0) $ modifyHealth Prop i' (1 - ph)
          returnI

cZombie = Card "zombie" 1 $ \[i,x] -> do
          i' <- toSlotNumber i
          oh <- getHealth Opp (255-i')
          unless (oh <= 0) $ fail "Slot is alive"
          putSlot Opp (255-i') $ Slot (-1) x
          returnI

allCards = [cI, cZero, cSucc, cDbl, cGet, cPut, cS, cK, cInc, cDec, cAttack,
        cHelp, cCopy, cRevive, cZombie]

-- xx: use Map?
cLookup :: String -> Card
cLookup name = head $ filter match allCards
    where match c = cardName c == name

------------------------------------------------------- Card support functions

getSlot :: Player -> SlotIdx -> State LTG Slot
getSlot p i = do
    let selector = if (p == Prop) then ltgProp else ltgOpp
    ltg <- get
    return $ (selector ltg) ! i


putSlot :: Player -> SlotIdx -> Slot -> State LTG ()
putSlot p i s = do
    ltg <- get
    put $ if (p == Prop)
        then ltg {ltgProp = (ltgProp ltg) // [(i, s)]}
        else ltg {ltgOpp  = (ltgOpp  ltg) // [(i, s)]}

putField :: Player -> SlotIdx -> Field -> State LTG ()
putField p i f = do
    s <- getSlot p i
    putSlot p i s {sField = f}

getField :: Player -> SlotIdx -> State LTG Field
getField p i = do
    (Slot _ f) <- getSlot p i
    return f

getHealth :: Player -> SlotIdx -> State LTG Int
getHealth p i = do
    (Slot h _) <- getSlot p i
    return h

modifyHealth :: Player -> Health -> SlotIdx -> State LTG ()
modifyHealth p dh i = do
    s@(Slot h _) <- getSlot p i
    let newH = min 65535 $ max 0 $ h + dh
    when (isAlive s) $ putSlot p i s {sHealth = newH}

isAlive :: Slot -> Bool
isAlive s = sHealth s > 0

returnI = return $ Function cI []

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


------------------------------------------------------- Game functions

incAppCounter :: State LTG ()
incAppCounter = do
    ltg@(LTG _ _ n _ _) <- get
    when (n == 1000) $ fail "Recursion depth exceeded"
    put $ ltg {ltgAppN = n + 1}

resetAppCounter :: State LTG ()
resetAppCounter = do
    ltg <- get
    put $ ltg {ltgAppN = 0}


applyCard :: AppOrder -> SlotIdx -> Card ->  State LTG ()
applyCard order i c = do
    resetAppCounter
    Slot h f <- getSlot Prop i
    when (h <= 0) $ fail "Slot is dead"
    f' <- case order of
            LeftApp  -> apply (Function c []) f
            RightApp -> apply f (Function c [])
    putField Prop i f'




swapPlayers :: LTG -> LTG
swapPlayers ltg =
    ltg {ltgProp = ltgOpp ltg, ltgOpp = ltgProp ltg, ltgPlayer = 1 - ltgPlayer ltg}

defaultHBoard = listArray (0, 255) (repeat $ Slot 10000 (Function cI []))
defaultLTG = LTG defaultHBoard defaultHBoard 0 1 0

incrementTurn :: LTG -> LTG
incrementTurn ltg = ltg {ltgTurn = 1 + ltgTurn ltg}

printHBoard :: Player -> LTG -> IO ()
printHBoard p ltg = do
    let slots = if (p == Prop) then ltgProp ltg else ltgOpp ltg
    let changed = filter hasChanged (assocs slots)
    mapM_ (putStrLn . format) changed
    where
        hasChanged (_, Slot 10000 (Function c [])) = c /= cI
        hasChanged _ = True

        format (i, Slot h f) = printf "%d={%d,%s}" i h (show f)


printLTG :: LTG -> IO ()
printLTG ltg = do
    putStrLn "ltgProp:"
    printHBoard Prop ltg
    putStrLn "ltgOpp:"
    printHBoard Opp ltg

