import LTG
import Control.Monad.State
import Control.Monad.Error

main = do
    let st' = execState (runErrorT actions) defaultLTG
    printHBoard Prop st'
    where actions = do
            applyCard LeftApp  1 $ cLookup "K"
            applyCard RightApp 2 $ cLookup "zero"
            applyCard LeftApp  2 $ cLookup "inc"

