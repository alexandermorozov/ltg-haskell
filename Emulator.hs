import Data.Array.MArray
import Data.Array.Diff
import Text.Printf
import Data.List
import Control.Monad.State

data    Card   = Card { cardName :: String
                      , cardN :: Int
                      , cardF :: ([Field] -> State LTG Field)
                      }

data    Field  = Value Int | Function Card [Field]

newtype Health = Health Int deriving (Show)
data    Slot   = Slot {sHealth :: Health, sField :: Field}
newtype HBoard = HBoard {hBoard :: DiffArray Int Slot}
data    LTG = LTG {opp :: HBoard, prop :: HBoard, appN :: Int}


-- getSlot i =

--addHealth i h = 0
--    ltg <- get

cI, cZero :: Card

cI = Card "I" 1 f
    where f [x] = return $ id x

cZero = Card "zero" 0 f
    where f [] = return $ Value 0

cSucc = Card "succ" 1 f
    where f [Value x] = return $ Value (x+1)

cK = Card "K" 2 f
    where f [x, y] = return x


getPropSlot i = do
    ltg <- get
    return $ (hBoard $ prop ltg) ! i

putPropSlot i s = do
    ltg <- get
    put $ ltg {prop = HBoard ((hBoard $ prop ltg) // [(i, s)])}

doApp :: Int -> State LTG ()
doApp si = do
    return ()

leftApp c si = do
    s <- getPropSlot si
    putPropSlot si $ Slot (sHealth s) (Function c [sField s])
    doApp si

rightApp si f = do
    return ()


--cInc = Card "inc" 1 f
--    where f [i] = addHealth i $ Health 1


createHBoard = HBoard $ listArray (0, 255) (repeat $ Slot (Health 10000) (Function cI []))

printHBoard :: HBoard -> IO ()
printHBoard (HBoard slots) = do
    let changed = filter hasChanged (assocs slots)
    mapM_ (putStrLn . format) changed 
    where
        hasChanged (i, Slot (Health 10000) (Function cI [])) = False
        hasChanged _ = True

        format (i, Slot (Health h) f) = printf "%d={%d,%s}" i h (formatField f)

        formatField (Value v) = show v
        formatField (Function c []) = cardName c
        formatField (Function c args) = (cardName c) ++
                "(" ++ (intercalate ", " $ map formatField args) ++ ")"


printLTG :: LTG -> IO ()
printLTG ltg = do
    putStrLn "prop"
    printHBoard $ prop ltg
    putStrLn "opp"
    printHBoard $ opp ltg

main = do
    let HBoard a = createHBoard
    let a' = a // [(1, Slot (Health 100) (Function cZero [])), (4, Slot (Health 10000) (Value 0))]
    let st = LTG createHBoard createHBoard 0
    let st' = (execState (leftApp cI 1) st) :: LTG
    printLTG st'
    printHBoard $ HBoard a'

    --putStrLn $ show b

