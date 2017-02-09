module Main where

import Control.Monad.Aff (Aff, launchAff, makeAff)
import Control.Monad.Eff (Eff, runPure)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.ST (ST, modifySTRef, newSTRef, readSTRef)
import Data.Either (Either(Left, Right))
import Data.Maybe (Maybe(..))
import Node.Buffer (Buffer, toString) as Buffer
import Node.Encoding (Encoding(..)) as Encoding
import Node.Process (stdin) as Process
import Node.Stream (Readable, onClose, onReadable, readEither) as Stream
import Prelude (Unit, (<$>), (<>), ($), bind, void)

toString
  :: Encoding.Encoding
  -> Maybe (Either String Buffer.Buffer)
  -> String
toString _ Nothing = "" -- ignore `null`
toString _ (Just (Left s)) = s
toString e (Just (Right b)) = (runPure $ unsafeCoerceEff $ Buffer.toString e b) -- ?

readStdin''
  :: forall e
   . Stream.Readable () (st :: ST String | e)
  -> Aff (st :: ST String | e) String
readStdin'' stdin = makeAff \_ ok -> do
  ref <- newSTRef ""
  Stream.onClose stdin $ void do
    s <- readSTRef ref
    ok s
  Stream.onReadable stdin $ void do
    c <- toString Encoding.UTF8 <$> Stream.readEither stdin Nothing
    modifySTRef ref \s -> s <> c

-- readStdin'
--   :: forall e
--    . Stream.Readable () e
--   -> Aff e String
-- readStdin' stdin = makeAff \_ ok -> do
--   Stream.onReadable stdin do
--     s <- toString Encoding.UTF8 <$> Stream.readEither stdin Nothing
--     ok s

-- readStdin
--    :: forall e
--    . Stream.Readable () ( err :: EXCEPTION | e )
--    -> Aff ( err :: EXCEPTION | e ) String
--  readStdin stdin = makeAff \_ ok -> do
--    Stream.onReadable stdin do
--      s <- fromMaybe "" <$> Stream.readString stdin Nothing Encoding.UTF8
--      ok s

main
  :: forall eff
   . Eff ( console :: CONSOLE, err :: EXCEPTION, st :: ST String | eff ) Unit
main = void $ launchAff do -- ignore canceler
  s <- readStdin'' Process.stdin
  liftEff $ log s
