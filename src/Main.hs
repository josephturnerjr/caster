import System.IO
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Chan
import qualified Control.Concurrent.BoundedChan as BC
import Control.Monad
import Control.Monad.Fix (fix)
import System.Locale
import Data.Time
import Data.Time.Format
 
type BChan = BC.BoundedChan

type ComponentStream = BChan
type Spout a = IO [a]
type Bolt a b = (a -> IO (Maybe b))

 
main :: IO ()
main = do
    ichan <- makeSpout $ fileSpout "test.txt"
    ochan <- makeBolt typeBolt ichan
    makeBolt printBolt ochan
    threadDelay 1000000

chanLimit = 1000

makeStream :: IO (ComponentStream a)
makeStream = BC.newBoundedChan chanLimit
emitStream = BC.writeChan
readStream = BC.readChan

makeSpout :: Spout a -> IO (ComponentStream a)
makeSpout s = do
    c <- makeStream
    l <- s
    forkIO $ BC.writeList2Chan c l
    return c

makeBolt :: Bolt a b -> ComponentStream a -> IO (ComponentStream b)
makeBolt b inChan = do
    outChan <- makeStream
    forkIO $ chanMap b inChan outChan
    return outChan

chanMap :: Bolt a b -> ComponentStream a -> ComponentStream b -> IO ()
chanMap f inChan outChan = fix $ \loop -> do
    a <- readStream inChan
    b <- f a
    maybe (return ()) (emitStream outChan) b
    loop

-- Bolt and Spout definitions

fileSpout :: String -> Spout String
fileSpout = fmap lines . readFile 

typeBolt :: Bolt String UTCTime
typeBolt ('#':_) = return Nothing
typeBolt line = do
    let (date:time:sIP:method:uri:query:port:username:cIP:userAgent:status:substatus:winStatus:timeTaken) = words line
    return $ Just (readTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" (date ++ " " ++ time) :: UTCTime)

printBolt :: Show a => Bolt a ()
printBolt a = fmap Just (putStrLn (show a))
