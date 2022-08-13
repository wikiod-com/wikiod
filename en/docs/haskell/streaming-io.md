---
title: "Streaming IO"
slug: "streaming-io"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## Streaming IO
[`io-streams`][1] is Stream-based library that focuses on the Stream abstraction but for IO. It exposes two types:

 * `InputStream`: a read-only smart handle

 * `OutputStream`: a write-only smart handle

We can create a stream with [`makeInputStream :: IO (Maybe a) -> IO (InputStream a)`][2]. Reading from a stream is performed using [`read :: InputStream a -> IO (Maybe a)`][3], where `Nothing` denotes an EOF:

```haskell
import Control.Monad (forever)
import qualified System.IO.Streams as S
import System.Random (randomRIO)

main :: IO ()
main = do
  is <- S.makeInputStream $ randomInt  -- create an InputStream
  forever $ printStream =<< S.read is  -- forever read from that stream
  return ()

randomInt :: IO (Maybe Int)
randomInt = do
  r <- randomRIO (1, 100)
  return $ Just r

printStream :: Maybe Int -> IO ()
printStream Nothing  = print "Nada!"
printStream (Just a) = putStrLn $ show a

```


  [1]: https://hackage.haskell.org/package/io-streams-1.3.5.0/docs/System-IO-Streams-Tutorial.html
  [2]: https://hackage.haskell.org/package/io-streams-1.3.5.0/docs/System-IO-Streams.html#g:3
  [3]: https://hackage.haskell.org/package/io-streams-1.3.5.0/docs/System-IO-Streams.html#v:read

