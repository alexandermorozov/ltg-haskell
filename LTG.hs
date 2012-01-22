module LTG
    (
      AppOrder (..)
    , LTG (ltgAppN, ltgTurn, ltgPlayer)
    , Card
    , SlotIdx
    , Player (..)
    , maxSlotIdx
    , cLookup
    , defaultLTG
    , applyCard
    , swapPlayers
    , incrementTurn
    , getHealth
    , getField
    , printHBoard
    , zombieScan
    , countAlive
    , cI, cZero, cSucc, cDbl, cPut, cS, cK, cInc, cDec, cAttack, cCopy
    , cRevive, cZombie
    ) where

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
                      , cardF :: ((Monad m, MonadState LTG m) => [Field] -> m Field)
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
                    , ltgOppZombies :: [SlotIdx]
                    , ltgPropZombies :: [SlotIdx]
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


maxSlotIdx = 255

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
          zombieInversion (-1) >>= modifyHealth Opp (maxSlotIdx - i')
          returnI

cAttack = Card "attack" 3 $ \[i,j,n] -> do
          i' <- toSlotNumber i
          n' <- toInt n
          ph <- getHealth Prop i'
          unlessZMode $ when (n' > ph) $ fail "Not enough vitality"
          modifyHealth Prop i' (-n')

          j' <- toSlotNumber j
          zombieInversion (-n'*9 `quot` 10) >>= modifyHealth Opp (maxSlotIdx - j')
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
          oh <- getHealth Opp (maxSlotIdx - i')
          unless (oh <= 0) $ fail "Slot is alive"
          putSlot Opp (maxSlotIdx - i') $ Slot (-1) x
          modify $ addZombie Opp (maxSlotIdx - i')
          returnI

allCards = [cI, cZero, cSucc, cDbl, cGet, cPut, cS, cK, cInc, cDec, cAttack,
        cHelp, cCopy, cRevive, cZombie]

-- xx: use Map?
cLookup :: String -> Card
cLookup name = head $ filter match allCards
    where match c = cardName c == name


------------------------------------------------------- Card support functions

getSlot :: (Monad m, MonadState LTG m) => Player -> SlotIdx -> m Slot
getSlot p i = liftM (getSlotRaw p i) get

putSlot :: (Monad m, MonadState LTG m) => Player -> SlotIdx -> Slot -> m ()
putSlot p i s = modify $ putSlotRaw p i s

modifySlot ::  (Monad m, MonadState LTG m) => Player -> SlotIdx -> (Slot -> Slot) -> m ()
modifySlot p i fn = liftM fn (getSlot p i) >>= putSlot p i

putField :: (Monad m, MonadState LTG m) => Player -> SlotIdx -> Field -> m ()
putField p i f = modifySlot p i $ \s -> s {sField = f}

getField :: (Monad m, MonadState LTG m) => Player -> SlotIdx -> m Field
getField p i = sField `liftM` getSlot p i

getHealth :: (Monad m, MonadState LTG m) => Player -> SlotIdx -> m Int
getHealth p i = sHealth `liftM` getSlot p i

putHealth :: (Monad m, MonadState LTG m) => Player -> SlotIdx -> Health -> m ()
putHealth p i h = modifySlot p i $ \s -> s {sHealth = h}

modifyHealth :: (Monad m, MonadState LTG m) => Player -> SlotIdx -> Health -> m ()
modifyHealth p i dh = do
    h <- getHealth p i
    let newH = min 65535 $ max 0 $ h + dh
    when (h > 0) $ putHealth p i newH

unlessZMode ::  (Monad m, MonadState LTG m) => m () -> m ()
unlessZMode ma = do
    zmode <- liftM ltgZombieMode get
    if zmode
        then return ()
        else ma

zombieInversion :: (Monad m, MonadState LTG m) => Int -> m Int
zombieInversion x = do
    zmode <- liftM ltgZombieMode get
    return (if zmode; then (-x); else x)

isAlive :: Slot -> Bool
isAlive s = sHealth s > 0

returnI :: (Monad m, MonadState LTG m) => m Field
returnI = return $ Function cI []

apply :: (Monad m, MonadState LTG m) => Field -> Field -> m Field
apply a b =
    case a of
        Value _ -> fail "Not a function"
        Function cA argsA ->
            case cardN cA - length argsA of
                0 -> fail "Native.Error" -- "Too many arguments"
                1 -> incAppCounter >> (cardF cA) (argsA ++ [b])
                _ -> return $ Function cA $ argsA ++ [b]

toIntSafe :: (Monad m, MonadState LTG m) => Field -> m (Maybe Int)
toIntSafe f =
    case f of
        Value x -> return $ Just x
        Function c args -> do
            if cardN c /= length args
                then return Nothing
                else (cardF c) args >>= toIntSafe


toInt :: (Monad m, MonadState LTG m) => Field -> m Int
toInt f =
    case f of
        Value x -> return x
        Function c args -> do
            when (cardN c /= length args) $ fail "Native.Error"
                        --"Cannot convert incomple f to int"
            f' <- (cardF c) args
            toInt f'

toSlotNumber :: (Monad m, MonadState LTG m) => Field -> m SlotIdx
toSlotNumber f = do
    i <- toInt f
    when (i > maxSlotIdx || i < 0) $ fail "Invalid slot number"
    return i

incAppCounter :: (Monad m, MonadState LTG m) => m ()
incAppCounter = do
    n <- ltgAppN `liftM` get
    when (n == 1000) $ fail "Native.AppLimitExceeded"
    modify $ \ltg -> ltg {ltgAppN = n + 1}


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

addZombie :: Player -> SlotIdx -> LTG -> LTG
addZombie p i ltg =
    case p of
        Prop -> ltg {ltgPropZombies = i:(ltgPropZombies ltg)}
        Opp  -> ltg {ltgOppZombies = i:(ltgOppZombies ltg)}

defaultHBoard = listArray (0, maxSlotIdx) (repeat $ Slot 10000 (Function cI []))


------------------------------------------------------- Game functions

defaultLTG = LTG defaultHBoard defaultHBoard [] [] 0 1 0 False

applyCard :: AppOrder -> Card -> SlotIdx -> WriterT [String] (State LTG) ()
applyCard order c i = do
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
    maybeZombies <- ltgPropZombies `liftM` get
    mapM_ helper (nub $ sort $ maybeZombies)
    modify $ \ltg -> ltg {ltgPropZombies = []}
    setZombieMode False
    where setZombieMode z = get >>= \l -> put $ l {ltgZombieMode = z}
          helper i = do
            s <- liftM (getSlotRaw Prop i) get
            when (sHealth s == -1) $ do
                tell ["applying zombie slot 1={-1," ++ (show $ sField s) ++ "} to I"]
                applyCard RightApp cI i
                resetField i
          resetField i = get >>=
            put . transformSlotRaw Prop i (\s -> Slot 0 (Function cI []))

countAlive :: WriterT [String] (State LTG) (Int, Int) -- player0, player1
countAlive = do
    ltg <- get
    let aprop = count $ ltgProp ltg
    let aopp  = count $ ltgOpp  ltg
    if ltgPlayer ltg == 0
        then return (aprop, aopp)
        else return (aopp, aprop)
    where count hb = length $ filter (\s -> sHealth s > 0) (elems hb)

swapPlayers :: WriterT [String] (State LTG) ()
swapPlayers = do
    ltg <- get
    put ltg {
        ltgProp = ltgOpp  ltg,
        ltgOpp  = ltgProp ltg,
        ltgPropZombies = ltgOppZombies  ltg,
        ltgOppZombies  = ltgPropZombies ltg,
        ltgPlayer = 1 - ltgPlayer ltg}

incrementTurn :: WriterT [String] (State LTG) ()
incrementTurn = get >>= put . \ltg -> ltg {ltgTurn = 1 + ltgTurn ltg}

printHBoard :: Player -> WriterT [String] (State LTG) ()
printHBoard p = do
    ltg <- get
    let slots = if (p == Prop) then ltgProp ltg else ltgOpp ltg
    let changed = filter hasChanged (assocs slots)
    mapM_ (tell . format) changed
    where
        hasChanged (_, Slot 10000 (Function c [])) = c /= cI
        hasChanged _ = True

        format (i, Slot h f) = [show i ++ "={" ++ show h ++ "," ++ show f ++ "}"]

