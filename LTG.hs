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

instance Show Field where
    showsPrec _ (Value v) = shows v
    showsPrec _ (Function c []) = shows $ cardName c
    showsPrec _ (Function c args) = (foldl' (.) (shows $ cardName c)
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
getSlot p i = do
    let selector = if (p == Prop) then ltgProp else ltgOpp
    ltg <- get
    return $ (selector ltg) ! i


putSlot :: Player -> SlotIdx -> Slot -> LTGRun ()
putSlot p i s = do
    ltg <- get
    put $ if (p == Prop)
        then ltg {ltgProp = (ltgProp ltg) // [(i, s)]}
        else ltg {ltgOpp  = (ltgOpp  ltg) // [(i, s)]}

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


------------------------------------------------------- Game functions

incAppCounter :: LTGRun ()
incAppCounter = do
    ltg@(LTG _ _ n _ _ _) <- get
    when (n == 1000) $ fail "Native.AppLimitExceeded"
    put $ ltg {ltgAppN = n + 1}

resetAppCounter :: LTGRun ()
resetAppCounter = do
    ltg <- get
    put $ ltg {ltgAppN = 0}


applyCard :: AppOrder -> SlotIdx -> Card ->  LTG -> (LTG, [String])
applyCard order i c ltg =
    let (err, ltg') = runState (runErrorT mainApp) ltg
    in case err of
        Right _ -> (ltg', [])
        Left e  -> (resetField i ltg',
                        ["Exception: " ++e, "slot " ++ show i ++ " reset to I"])
    where mainApp = do
            resetAppCounter
            Slot h f <- getSlot Prop i
            unlessZMode $ when (h <= 0) $ fail "Native.Error"
            f' <- case order of
                    LeftApp  -> apply (Function c []) f
                    RightApp -> apply f (Function c [])
            putField Prop i f'

resetField :: SlotIdx -> LTG -> LTG
resetField i = snd . runState (runErrorT $ returnI >>= putField Prop i)

zeroHealth :: SlotIdx -> LTG -> LTG
zeroHealth i = snd . runState (runErrorT $ putHealth Prop i 0)


-- scans only proponent field
zombieScan :: LTG -> (LTG, [String])
zombieScan ltg = let (ltg', msgs) = helper (ltg {ltgZombieMode=True}) [] 0
                 in (ltg' {ltgZombieMode=False}, msgs)
    where helper ltg msgs 256 = (ltg, msgs)
          helper ltg msgs i =
            let s = ltgProp ltg ! i
            in if sHealth s /= -1
                   then helper ltg msgs (i+1)
                   else let msg = "applying zombie slot 1={-1," ++
                                    (show $ sField s) ++ "} to I"
                            (ltg', msgs') = applyCard RightApp i cI ltg
                            ltg'' = resetField i $ zeroHealth i ltg'
                            -- TODO: reset
                        in helper ltg'' (msgs ++ [msg] ++ msgs') (i+1)

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

defaultHBoard = listArray (0, 255) (repeat $ Slot 10000 (Function cI []))
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

