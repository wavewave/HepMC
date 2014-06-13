import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Strict
import qualified Data.Attoparsec.Text as A
import           Data.Text 
import qualified Data.Text.IO as TIO
-- import           Pipes (await, runEffect, yield, (>->))
import           Pipes
import qualified Pipes.Prelude
import qualified Pipes.Attoparsec as PA
import qualified Pipes.Parse as PP
import qualified Pipes.Text.IO as PIO
import System.Environment
import System.IO (withFile, IOMode(..),  stdout)
-- 
import HEP.Parser.HepMC
--

takeWait :: (Monad m) => Int -> Pipe a a m ()
takeWait n = worker 0 
  where worker m 
          | m > n = await >> worker (m+1)  
          | otherwise = await >>= yield >> worker (m+1)        

main = do 
  putStrLn "hepmc tester" 
  args <- getArgs 
  let filename = args !! 0
      p = PA.parse hepmcHeader
  print filename 
  withFile filename ReadMode $ \hin -> 
    runEffect $ do
      (r1,s) <- lift (runStateT p (PIO.fromHandle hin))
      action s >-> Pipes.Prelude.tee (counter 100 0 >-> forever (await >> return ())) 
        >-> takeWait 500 >-> counter 70 0 >-> (replicateM 3 printer >> forever (await >> return ())) -- printer
  return ()

evp :: (Monad m) => PP.Parser Text m (Maybe (Either PA.ParsingError (GenEvent,NamedWeight,MomentumPositionUnit,[Int])))
evp = PA.parse event

action :: (Monad m) => Producer Text m () -> Producer (GenEvent,NamedWeight,MomentumPositionUnit,[Int]) m () 
action s = do
  (r,s') <- lift (runStateT evp s)
  case r of
    Nothing -> return ()
    Just (Left err) -> return ()
    Just (Right ev) -> yield ev >> action s'

 
counter :: (MonadIO m, Monad m) => Int -> Int -> Pipe a a m () 
counter m n = do 
  e <- await
  if n `mod` m == 0 then liftIO (print n) else return ()  
  yield e 
  counter m (n+1)


printer :: (MonadIO m, Monad m, Show a, Show b, Show c) => Consumer (a,b,c,d) m () 
printer = do
  (a,b,c,_) <- await 
  liftIO (print (a,b,c))




-- Consumer (Text, [Int]) m ()

