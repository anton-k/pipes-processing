{-# Language ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances, DeriveFunctor #-}
module Gen where

import Control.Applicative
import Control.Monad.Trans.Reader
import Pipes
import qualified Pipes.Prelude as P
import Control.Monad
import System.Random
import Data.Array
import Control.Monad.IO.Class

type SampleRate = Float

newtype Env a = Env { unEnv :: ReaderT SampleRate IO a }
    deriving (Functor)

instance Applicative Env where
    pure = Env . pure
    Env mf <*> Env ma = Env (mf <*> ma)

instance Monad Env where
    return = Env . return
    Env ma >>= mf = Env (ma >>= unEnv . mf)

instance MonadIO Env where
    liftIO = Env . lift

instance Monoid (Evt a) where
    mempty = act $ return Nothing
    ma `mappend` mb = undefined    

type Fun a b = Pipe a b Env ()
type Gen a = Producer a Env ()

type Sig = Gen Float

type Evt a = Gen (Maybe a)
type Tick  = Evt ()

instance Num a => Num (Gen a) where
    fromInteger = act . return . fromInteger
    (+) = P.zipWith (+)
    (*) = P.zipWith (*)
    negate a = a >-> P.map negate
    abs a = a >-> P.map abs
    signum a = a >-> P.map signum

instance Fractional a => Fractional (Gen a) where
    fromRational = act . return . fromRational
    recip a = a >-> P.map recip

act :: Env a -> Gen a
act x = forever $ do    
    yield =<< lift x  

iter' :: forall a . a -> (a -> Env a) -> Gen a
iter' s0 f = P.unfoldr (\a -> fmap (\x -> ((Right (a, x)) :: Either () (a, a))) $ f a) s0

iter :: a -> (a -> a) -> Gen a
iter a f = iter' a (return . f)

iterTime :: a -> (Float -> a -> a) -> Gen a
iterTime s0 f = iter' (0, s0) (\(t, a) -> Env $ ReaderT $ \dt -> return (t + dt, f t a)) >-> P.map snd

ints :: Gen Int
ints = iter 0 succ

randInts :: (Int, Int) -> Gen Int
randInts interval = act (liftIO $ randomRIO interval) 

oneOf :: [a] -> Gen a
oneOf as = randInts interval >-> P.map (arr !)
    where
        arr = listArray interval as
        interval = (0, length as - 1)
    

------------------------------------------------

type Stamp a = (Float, a)

osc :: Sig -> Sig
osc cps = cps >-> mapTime (\t cps -> sin (t * cps))

phasorFun :: (Float -> Float) -> Sig -> Sig 
phasorFun f cps = phasor cps >-> P.map f

saw :: Sig -> Sig
saw = phasorFun $ \x -> -1 + 2 * x

tri :: Sig -> Sig
tri = phasorFun f
    where
        f x 
            | x < 0.25  = 4 * x
            | x > 0.75  = -1 + 4 * (x - 0.75)
            | otherwise = 1 - 2 * (x - 0.25)

sqr :: Sig -> Sig
sqr = phasorFun $ \x -> if (x < 0.5) then -1 else 1 

noise :: Sig
noise = act $ liftIO $ randomRIO (-1, 1)

lins :: Float -> Evt (Stamp Float) -> Sig
lins v0 evts = evts >-> scanStep f s0 get
    where
        f dt s a = case a of 
            Nothing -> onNothing dt s
            Just x -> onNext dt x s

        onNothing dt (y, k, timeToGoal)
            | timeToGoal <= 0 = (y, k, 0)
            | otherwise       = (y + k, k, timeToGoal - dt)

        onNext dt (y1, dur) (y, _, _) = onNothing dt (y, k, dur)
            where k = (y1 - y) / dur

        s0 = (v0, 0, 0)

        get (y, _, _) = y
 
linloop :: [Stamp Float] -> Sig -> Sig
linloop = undefined

linseg :: [Stamp Float] -> Sig -> Sig
linseg = undefined

schedEvt :: [Stamp a] -> Evt a
schedEvt = undefined

cfd :: Sig -> Sig -> Sig -> Sig
cfd k a b = k * a + (1 - k) * b   -- zipWith3 (\k a b -> k * a + (1 - k) * b) sk sa sb

phasor :: Sig -> Sig
phasor cps = cps >-> scanStep (\dt t freq -> snd $ properFraction $ t + dt * freq) 0 id

type Next a = Producer a IO ()

onGen :: Gen a -> Tick -> Evt a
onGen gen ticks = ticks >-> P.scanM f s0 get >-> P.drop 1
    where
        f x@(s, gen1) a = maybe (return $ (Nothing, gen1)) (const $ update gen1) a 

        s0 = return (Nothing, gen)
        update gs = fmap (\x -> (x, gs >-> P.drop 1)) $ P.head gs

        get = return . fst

everyN :: Int -> Tick 
everyN size = iter fire (\(n, _) -> if n == size - 1 then fire else none (n + 1)) >-> P.map snd 
    where
        fire = (0, Just ())
        none n = (n, Nothing)


metro :: Sig -> Tick
metro dur = dur >-> P.map recip >-> scanStep (\dt (t, _) freq -> checkZeroes $ t + dt * freq) (fire 0) id >-> P.map snd
    where
        checkZeroes t 
            | t >= 1    = fire t
            | otherwise = none t

        fire t = (snd $ properFraction t, Just ())
        none t = (t, Nothing)

randMetro :: Sig -> Sig -> Tick
randMetro disp dur = metro (dur + disp * noise)

accumEvt :: b -> (b -> a -> b) -> Evt a -> Gen b
accumEvt s0 f evts = evts >-> go
    where
        go = P.scan (\s a -> maybe s (\x -> f s x) a) s0 id

----------------------------------------------------------

scanStep :: (Float -> x -> a -> x) -> x -> (x -> b) -> Fun a b
scanStep f s0 get = P.scanM (\s a -> Env $ ReaderT $ \dt -> return $ f dt s a) (return s0) (return . get)

scanTime :: (Float -> x -> a -> x) -> x -> (x -> b) -> Fun a b
scanTime f s0 get = P.scanM (\(t, v) a -> Env $ ReaderT $ \dt -> return $ (t + dt, f t v a)) (return (0, s0)) (return . get . snd)

mapTime :: (Float -> a -> b) -> Fun a b
mapTime f = scanTime (\t _ a -> f t a) undefined id >-> P.drop 1

time :: Gen Float
time = iterTime 0 (\t _ -> t)

---------------------------------------------------------------------------

run :: Show a => Gen a -> IO ()
run p = runEnvSr 60 (runEffect $ for (p >-> P.take 10) (lift . printEnv))

printEnv :: Show a => a -> Env ()
printEnv = liftIO . print

runEnvSr :: SampleRate -> Env a -> IO a
runEnvSr sr (Env a) = runReaderT a (1 / sr)

runEnv :: Env a -> Float -> IO a
runEnv (Env a) dt = runReaderT a dt