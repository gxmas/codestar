module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Hedgehog ((===))
import qualified Hedgehog as H

import Streaming.Core

main :: IO ()
main = defaultMain $ testGroup "streaming-core"
  [ testProperty "write then read round-trips a chunk" prop_roundTrip
  , testProperty "closed stream returns ReadEndOfStream" prop_closedStream
  , testProperty "paused stream write returns WriteBackpressure when full" prop_pausedWrite
  ]

prop_roundTrip :: H.Property
prop_roundTrip = H.property $ do
  s <- H.evalIO (createStream defaultStreamConfig)
  let chunk = StreamChunk { chunkData = (42 :: Int), chunkSequence = 1, chunkFinal = False }
  wr <- H.evalIO (writeChunk s chunk)
  wr === WriteOk
  rr <- H.evalIO (readChunk s)
  case rr of
    ReadOk c -> c.chunkData === 42
    _        -> H.failure

prop_closedStream :: H.Property
prop_closedStream = H.property $ do
  s <- H.evalIO (createStream defaultStreamConfig :: IO (Stream Int))
  H.evalIO (closeStream s)
  rr <- H.evalIO (readChunk s)
  rr === ReadEndOfStream

prop_pausedWrite :: H.Property
prop_pausedWrite = H.property $ do
  let cfg = StreamConfig { bufferSize = 1, backpressureEnabled = True }
  s <- H.evalIO (createStream cfg :: IO (Stream Int))
  let chunk n = StreamChunk { chunkData = n, chunkSequence = n, chunkFinal = False }
  _ <- H.evalIO (writeChunk s (chunk 1))
  wr <- H.evalIO (writeChunk s (chunk 2))
  wr === WriteBackpressure
