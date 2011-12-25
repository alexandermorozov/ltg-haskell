import Data.Array.MArray
import Data.Array.Diff
import Text.Printf
import Data.List

data    Card   = Card {cardName :: String, cardN :: Int, cardF :: ([Field] -> Field)}
data    Field  = Value Int | Function Card [Field]
newtype Health = Health Int deriving (Show)
data    Slot   = Slot Health Field
newtype HBoard = HBoard (DiffArray Int Slot)


cI = Card "I" 1 f
    where f [x] = id x

cZero = Card "Zero" 0 f
    where f [] = Value 0

cSucc = Card "Succ" 1 f
    where f [Value x] = Value (x+1)

cK = Card "K" 2 f
    where f [x, y] = x

makeHBoard = HBoard $ listArray (0, 255) (repeat $ Slot (Health 10000) (Function cI []))

printHBoard :: HBoard -> IO ()
printHBoard (HBoard slots) = do
    let changed = filter hasChanged (assocs slots)
    mapM_ (putStrLn . format) changed 
    where
        hasChanged (i, Slot (Health 10000) (Function cI [])) = False
        hasChanged _ = True
        format (i, Slot (Health h) f) = printf "%d %d %s" i h (formatField f)
        formatField (Value v) = show v
        formatField (Function c args) = (cardName c) ++
                "(" ++ (intercalate ", " $ map formatField args) ++ ")"


main = do
    let HBoard a = makeHBoard
    let a' = a // [(1, Slot (Health 100) (Function cZero [])), (4, Slot (Health 10000) (Value 0))]
    printHBoard $ HBoard a'
    --putStrLn $ show b

