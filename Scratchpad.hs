import LTG
import Control.Monad.State

main = do
    let st' = (flip execState) defaultLTG $ do
        rightApp 1 $ cLookup "K"
        rightApp 2 $ cLookup "zero"
        leftApp  2 $ cLookup "dec"
    printLTG st'

