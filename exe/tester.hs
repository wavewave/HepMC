
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Strict
import qualified Data.Attoparsec.Text as A
import qualified Data.Text.IO as TIO
import           Pipes (await, runEffect, (>->))
import qualified Pipes.Attoparsec as PA
import qualified Pipes.Text.IO as PIO
import System.Environment
import System.IO (withFile, IOMode(..),  stdout)
-- 
import HEP.Parser.HepMC



main = do 
  putStrLn "hepmc tester" 
  args <- getArgs 
  let filename = args !! 0

      p = PA.parse hepmc
  print filename 
  withFile filename ReadMode $ \hin -> 
    runEffect $ do 
      -- p' <- 
      r <- evalStateT p (PIO.fromHandle hin)
     
      liftIO $ print r
      -- p' >-> lift (PIO.toHandle stdout)
  -- print x 
  -- str <- TIO.readFile filename 
  -- print (parse hepmc str)
  return ()
  

