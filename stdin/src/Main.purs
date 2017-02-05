module Main where

import Control.Monad.Aff (Aff, makeAff, runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Maybe (Maybe(..), fromMaybe)
import Node.Encoding (Encoding(..)) as Encoding
import Node.Process (stdin) as Process
import Node.Stream (Readable, onReadable, readString) as Stream
import Prelude (Unit, (<$>), ($), bind, const, pure, unit, void)

readStdin
  :: forall e
   . Stream.Readable () ( err :: EXCEPTION | e )
  -> Aff ( err :: EXCEPTION | e ) String
readStdin stdin = makeAff \_ ok -> do
  Stream.onReadable stdin do
    s <- fromMaybe "" <$> Stream.readString stdin Nothing Encoding.UTF8
    ok s

main
  :: forall eff
   . Eff ( console :: CONSOLE, err :: EXCEPTION | eff ) Unit
main = void $ runAff (const (pure unit)) (\s -> log s) $ readStdin Process.stdin
