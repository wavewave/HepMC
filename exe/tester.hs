module Main where

import           Control.Monad                    (forever, replicateM_, when)
import           Control.Monad.Trans.State.Strict
import           Data.Text                        (Text)
import           Pipes
import           Pipes.Attoparsec                 (ParsingError (..), parse)
import           Pipes.Parse                      (Parser)
import qualified Pipes.Prelude                    as P
import qualified Pipes.Text.IO                    as PIO
import           System.Environment               (getArgs)
import           System.IO                        (IOMode (..), withFile)
--
import           HEP.Parser.HepMC.Parser
import           HEP.Parser.HepMC.Type
--

main :: IO ()
main = do
  putStrLn "-- This is hepmc tester."
  args <- getArgs
  let infile = head args
  putStrLn $ "---- The input file is " ++ show infile ++ "."
  withFile infile ReadMode $ \hin ->
    runEffect $ hepmcEvent hin >-> P.take 3 >-> P.print
  putStrLn "---- Done."

main' :: IO ()
main' = do
  putStrLn "hepmc tester"
  args <- getArgs
  let filename = head args
      p = parse hepmcHeader
  print filename
  withFile filename ReadMode $ \hin ->
    runEffect $ do
      (_, s) <- lift (runStateT p (PIO.fromHandle hin))
      action s
      >-> P.tee (counter 100 0 >-> forever (void await))
      >-> takeWait 500
      >-> counter 70 0
      >-> (replicateM_ 3 printer >> forever (void await)) -- printer

takeWait :: Monad m => Int -> Pipe a a m ()
takeWait n = worker 0
  where worker m | m > n     = await >> worker (m+1)
                 | otherwise = await >>= yield >> worker (m+1)

evp :: Monad m => Parser Text m (Maybe (Either ParsingError GenEvent))
evp = parse event

action :: Monad m => Producer Text m () -> Producer GenEvent m ()
action s = do
  (r, s') <- lift (runStateT evp s)
  case r of
    Nothing         -> return ()
    Just (Left _)   -> return ()
    Just (Right ev) -> yield ev >> action s'

counter :: (MonadIO m, Monad m) => Int -> Int -> Pipe a a m ()
counter m n = do
  e <- await
  when (n `mod` m == 0) $ (liftIO . print) n
  yield e
  counter m (n+1)

printer :: (MonadIO m, Monad m, Show a) => Consumer a m ()
printer = await >>= liftIO . print
