import Data.Array.MArray
import Data.Array.Diff
import Text.Printf

data    Field  = Value Int | Card Card deriving (Show)
newtype Health = Health Int deriving (Show)
data    Slot   = Slot Health Field deriving (Show)
newtype HBoard = HBoard (DiffArray Int Slot) deriving (Show)

data    Card   = I
               | Zero
               | Succ Field
               | S Field Field Field
               deriving (Show)



makeHBoard = HBoard $ listArray (0, 255) (repeat $ Slot (Health 10000) (Card I))

printHBoard :: HBoard -> IO ()
printHBoard (HBoard slots) = do
    let changed = filter hasChanged (assocs slots)
    mapM_ (putStrLn . format) changed 
    where
        hasChanged (i, Slot (Health 10000) (Card I)) = False
        hasChanged _ = True
        format (i, Slot (Health h) f) = printf "%d %d %s" i h (show f)

main = do
    let HBoard a = makeHBoard
    let a' = a // [(1, Slot (Health 100) (Card Zero)), (4, Slot (Health 200) (Card $ Succ))]
    printHBoard $ HBoard a'
    --putStrLn $ show b

