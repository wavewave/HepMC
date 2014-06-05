
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Strict
import qualified Data.Attoparsec.Text as A
import           Data.Text
import qualified Data.Text.IO as TIO
-- import           Pipes (await, runEffect, yield, (>->))
import           Pipes
import qualified Pipes.Attoparsec as PA
import qualified Pipes.Parse as PP
import qualified Pipes.Text.IO as PIO
import System.Environment
import System.IO (withFile, IOMode(..),  stdout)
-- 
import HEP.Parser.HepMC


{- 
main = do 
  putStrLn "hepmc tester" 
  args <- getArgs 
  let filename = args !! 0
  str <- TIO.readFile filename 
  print (A.parse hepmcHeader str)
-}

main = do 
  putStrLn "hepmc tester" 
  args <- getArgs 
  let filename = args !! 0
      p = PA.parse hepmcHeader
      -- evp = PA.parse event
  print filename 
  withFile filename ReadMode $ \hin -> 
    runEffect $ do
      (r1,s) <- lift (runStateT p (PIO.fromHandle hin))
      forever (action s) >-> printpipe 0
      -- r2 <- evalStateT evp s 
      -- liftIO $ print (r1,r2)
      -- p' >-> lift (PIO.toHandle stdout)
  -- print x 
  -- str <- TIO.readFile filename 
  -- print (parse hepmc str)
  return ()
-- flip runStateT (PIO.fromHandle hin) $ (p >> forever action) >-> return ()


evp :: (Monad m) => PP.Parser Text m (Maybe (Either PA.ParsingError (Text, [Int])))
evp = PA.parse event

-- action :: Producer Text (Producer Double m) r
action :: (Monad m) => Producer Text m () -> Producer (Text, [Int]) m () 
action s = do
  (r,s') <- lift (runStateT evp s)
  case r of
    Nothing -> return ()
    Just (Left err) -> return ()
    Just (Right ev) -> yield ev >> action s'

 

printpipe :: (MonadIO m, Monad m) => Int -> Consumer (Text, [Int]) m ()
printpipe n = do 
  ev <- await
  if n `mod` 100 == 0 then liftIO (print n) else return ()  
  -- liftIO $ print ev 
  printpipe (n+1)

