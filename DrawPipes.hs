module DrawPipes(
    runGen
) where

import Gen
import Graphics.Proc
import Pipes
import qualified Pipes.Prelude as P
import Control.Monad.IO.Class
import Data.Default

type TimeInterval = Float

runGen :: Draw -> (s -> Draw) -> Gen s -> IO ()
runGen initScene draw gen = runProc $ def 
    { procSetup = setup
    , procUpdateTime = updateTime   
    , procDraw = draw' }
    where
        setup = do
            initScene
            getNextState initDt gen
        
        updateTime dt maybeSt = maybe halt go maybeSt
            where
                go (_, g) = getNextState dt g
                halt = return Nothing

        draw' maybeSt = maybe (return ()) (draw . fst) maybeSt

initDt = 1 / 60

getNextState :: TimeInterval -> Gen s -> Pio (Maybe (s, Gen s))
getNextState dt gen = liftIO $ do
    res <- runEnv (next gen) dt
    return $ case res of
        Left _  -> Nothing
        Right a -> Just a

