import Data.Array.MArray
import Data.Array.Diff
import Text.Printf

data    Card   = I | Zero | Succ deriving (Show)
newtype Health = Health Int deriving (Show)
data    Slot   = Slot Health [Card] deriving (Show)
newtype HBoard = HBoard (DiffArray Int Slot) deriving (Show)

makeHBoard = HBoard $ listArray (0, 255) (repeat $ Slot (Health 10000) [])

printHBoard :: HBoard -> IO ()
printHBoard (HBoard slots) = do
    let changed = filter hasChanged (assocs slots)
    mapM_ (putStrLn . format) changed 
    where
        hasChanged (i, Slot (Health 10000) []) = False
        hasChanged _ = True
        format (i, Slot (Health h) cards) = printf "%d %d %s" i h (show cards)

main = do
    -- I hope that compiler won't optimise two instances into one 
    let HBoard a = makeHBoard
    let a' = a // [(1, Slot (Health 100) []), (4, Slot (Health 200) [Zero])]
    printHBoard $ HBoard a'
    --putStrLn $ show b

