import Data.Array.MArray
import Data.Array.Diff
import Text.Printf

newtype Health = Health Int deriving (Show)
data    Field = Value Int | Function (Field -> Field)
data    Slot   = Slot Health Field
newtype HBoard = HBoard (DiffArray Int Slot)


cI = Value 0
cZero = Function id
cSucc = Function (\(Value x) -> Value (x-1))
cK = Function (\x -> (Function (\y -> x)))

makeHBoard = HBoard $ listArray (0, 255) (repeat $ Slot (Health 10000) (cI))

printHBoard :: HBoard -> IO ()
printHBoard (HBoard slots) = do
    let changed = filter hasChanged (assocs slots)
    mapM_ (putStrLn . format) changed 
    where
        hasChanged (i, Slot (Health 10000) (cI)) = False
        hasChanged _ = True
        format (i, Slot (Health h) f) = printf "%d %d" i h -- (show f)

main = do
    let HBoard a = makeHBoard
    let a' = a // [(1, Slot (Health 100) (cI)), (4, Slot (Health 10000) (cZero))]
    printHBoard $ HBoard a'
    --putStrLn $ show b

