module LTG
    (
      AppOrder (..)
    , LTG (ltgAppN, ltgTurn, ltgPlayer)
    , Card
    , SlotIdx
    , Player (..)
    , cLookup
    , defaultLTG
    , applyCard
    , swapPlayers
    , incrementTurn
    , printHBoard
    , zombieScan
    , countAlive
    ) where

{- TODO:
 * fix chaos: some function are supposed to run inside monad, others
     are executed without. Functionality significantly overlaps,
     sometimes it's difficult to tell which is which...
-}
import Data.Array
import Text.Printf
import Data.List (intercalate)
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Writer
import Debug.Trace
import Data.List

data    Card   = Card { cardName :: String
                      , cardN :: Int
                      , cardF :: ([Field] -> LTGRun Field)
                      }

data    Field  = Value Int | Function Card [Field]

type Health   = Int
type SlotIdx  = Int
data Slot     = Slot { sHealth :: Health, sField :: Field }
type HBoard   = Array SlotIdx Slot
data AppOrder = LeftApp | RightApp
data Player   = Opp | Prop deriving (Eq)
data LTG      = LTG { ltgOpp :: HBoard
                    , ltgProp :: HBoard
                    , ltgAppN :: Int -- xx: not sure if it should be here
                    , ltgTurn :: Int
                    , ltgPlayer :: Int -- 0 or 1
                    , ltgZombieMode :: Bool
                    }

type LTGRun a = ErrorT String (State LTG) a

instance Eq Card where
    c == c' = cardName c == cardName c'

instance Show Card where
    show c = cardName c

-- xx: not sure at all if I got this right, need one more pass
instance Show Field where 
    show a = shows a ""
    showsPrec _ (Value v) = shows v
    showsPrec _ (Function c []) = (++) (cardName c)
    showsPrec _ (Function c args) = (foldl' (.) ((++) $ cardName c)
                    (map (\x -> ('(':) . (shows x) . (')':)) args))


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
              else fail "Native.Error"

cPut = Card "put" 1 $ \[_] -> returnI

cS = Card "S" 3 $ \[f,g,x] -> do
          h <- apply f x
          y <- apply g x
          apply h y

cK = Card "K" 2 $ \[x,y] -> return x

cInc = Card "inc" 1 $ \[i] -> do
          i' <- toSlotNumber i
          zombieInversion 1 >>= modifyHealth Prop i'
          returnI

cDec = Card "dec" 1 $ \[i] -> do
          i' <- toSlotNumber i
          zombieInversion (-1) >>= modifyHealth Opp i'
          returnI

cAttack = Card "attack" 3 $ \[i,j,n] -> do
          i' <- toSlotNumber i
          n' <- toInt n
          ph <- getHealth Prop i'
          unlessZMode $ when (n' > ph) $ fail "Not enough vitality"
          modifyHealth Prop i' (-n')

          j' <- toSlotNumber j
          zombieInversion (-n'*9 `quot` 10) >>= modifyHealth Opp (255-j')
          returnI

cHelp = Card "help" 3 $ \[i,j,n] -> do
          i' <- toSlotNumber i
          n' <- toInt n
          ph <- getHealth Prop i'
          when (n' > ph) $ fail "Not enough vitality"
          modifyHealth Prop i' (-n')

          j' <- toSlotNumber j
          zombieInversion (n'*11 `quot` 10) >>= modifyHealth Prop j'
          returnI

cCopy = Card "copy" 1 $ \[i] -> do
          i' <- toSlotNumber i
          getField Opp i'

cRevive = Card "revive" 1 $ \[i] -> do
          i' <- toSlotNumber i
          ph <- getHealth Prop i'
          when (ph <= 0) $ putHealth Prop i' 1
          returnI

cZombie = Card "zombie" 2 $ \[i,x] -> do
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

getSlot :: Player -> SlotIdx -> LTGRun Slot
getSlot p i = liftM (getSlotRaw p i) get

putSlot :: Player -> SlotIdx -> Slot -> LTGRun ()
putSlot p i s = get >>= put . putSlotRaw p i s

putField :: Player -> SlotIdx -> Field -> LTGRun ()
putField p i f = do
    s <- getSlot p i
    putSlot p i s {sField = f}

getField :: Player -> SlotIdx -> LTGRun Field
getField p i = do
    (Slot _ f) <- getSlot p i
    return f

getHealth :: Player -> SlotIdx -> LTGRun Int
getHealth p i = do
    (Slot h _) <- getSlot p i
    return h

putHealth :: Player -> SlotIdx -> Health -> LTGRun ()
putHealth p i h = do
    s <- getSlot p i
    putSlot p i s {sHealth = h}

modifyHealth :: Player -> SlotIdx -> Health -> LTGRun ()
modifyHealth p i dh = do
    s@(Slot h _) <- getSlot p i
    zmode <- liftM ltgZombieMode get
    let newH = min 65535 $ max 0 $ h + dh
    when (isAlive s) $ putSlot p i s {sHealth = newH}

unlessZMode :: LTGRun () -> LTGRun ()
unlessZMode ma = do
    zmode <- liftM ltgZombieMode get
    if zmode
        then return ()
        else ma

zombieInversion :: Int -> LTGRun Int
zombieInversion x = do
    zmode <- liftM ltgZombieMode get
    return (if zmode; then (-x); else x)

isAlive :: Slot -> Bool
isAlive s = sHealth s > 0

returnI = return $ Function cI []

apply :: Field -> Field -> LTGRun Field
apply a b =
    case a of
        Value _ -> fail "Not a function"
        Function cA argsA ->
            case cardN cA - length argsA of
                0 -> fail "Native.Error" -- "Too many arguments"
                1 -> incAppCounter >> (cardF cA) (argsA ++ [b])
                _ -> return $ Function cA $ argsA ++ [b]

toInt :: Field -> LTGRun Int
toInt f =
    case f of
        Value x -> return x
        Function c args -> do
            when (cardN c /= length args) $ fail "Native.Error"
                        --"Cannot convert incomple f to int"
            f' <- (cardF c) args
            toInt f'

toSlotNumber :: Field -> LTGRun SlotIdx
toSlotNumber f = do
    i <- toInt f
    when (i > 255 || i < 0) $ fail "Invalid slot number"
    return i

incAppCounter :: LTGRun ()
incAppCounter = do
    ltg@(LTG _ _ n _ _ _) <- get
    when (n == 1000) $ fail "Native.AppLimitExceeded"
    put $ ltg {ltgAppN = n + 1}


------------------------------------------------------- Low-level slot operations

getSlotRaw :: Player -> SlotIdx -> LTG -> Slot
getSlotRaw p i ltg =
    let selector = if (p == Prop) then ltgProp else ltgOpp
    in (selector ltg) ! i

putSlotRaw :: Player -> SlotIdx -> Slot -> LTG -> LTG
putSlotRaw p i s ltg =
    if (p == Prop)
        then ltg {ltgProp = (ltgProp ltg) // [(i, s)]}
        else ltg {ltgOpp  = (ltgOpp  ltg) // [(i, s)]}

transformSlotRaw :: Player -> SlotIdx -> (Slot -> Slot) -> LTG -> LTG
transformSlotRaw p i f ltg =
    let s = getSlotRaw Prop i ltg
    in  putSlotRaw Prop i (f s) ltg

defaultHBoard = listArray (0, 255) (repeat $ Slot 10000 (Function cI []))


------------------------------------------------------- Game functions

applyCard :: AppOrder -> SlotIdx -> Card -> WriterT [String] (State LTG) ()
applyCard order i c = do
    ltg <- get
    let (err, ltg') =  runState (runErrorT mainApp) ltg {ltgAppN = 0}
    put ltg'
    case err of
        Right _ -> return ()
        Left e  -> do
            resetField
            tell ["Exception: " ++ e]
            tell ["slot " ++ show i ++ " reset to I"]

    where mainApp = do
            Slot h f <- getSlot Prop i
            unlessZMode $ when (h <= 0) $ fail "Native.Error"
            f' <- case order of
                    LeftApp  -> apply (Function c []) f
                    RightApp -> apply f (Function c [])
            putField Prop i f'

          resetField = get >>= 
            put . transformSlotRaw Prop i (\s -> s {sField = Function cI []})

-- scans only proponent's field
zombieScan :: WriterT [String] (State LTG) ()
zombieScan = do
    setZombieMode True
    sequence $ map helper [0..255]
    setZombieMode False
    where setZombieMode z = get >>= \l -> put $ l {ltgZombieMode = z}
          helper i = do
            s <- liftM (getSlotRaw Prop i) get
            when (sHealth s == -1) $ do
                tell ["applying zombie slot 1={-1," ++ (show $ sField s) ++ "} to I"]
                applyCard RightApp i cI
                resetField i
          resetField i = get >>=
            put . transformSlotRaw Prop i (\s -> Slot 0 (Function cI []))

countAlive :: LTG -> (Int, Int) -- player0, player1
countAlive ltg =
    let aprop = count $ ltgProp ltg
        aopp  = count $ ltgOpp  ltg
    in if ltgPlayer ltg == 0
           then (aprop, aopp)
           else (aopp, aprop)
    where count hb = length $ filter (\s -> sHealth s > 0) (elems hb)

swapPlayers :: LTG -> LTG
swapPlayers ltg =
    ltg {ltgProp = ltgOpp ltg, ltgOpp = ltgProp ltg, ltgPlayer = 1 - ltgPlayer ltg}

defaultLTG = LTG defaultHBoard defaultHBoard 0 1 0 False

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

        format (i, Slot h f) = show i ++ "={" ++ show h ++ "," ++ show f ++ "}"

