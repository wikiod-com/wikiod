---
title: "Attoparsec"
slug: "attoparsec"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

Attoparsec is a parsing combinator library that is "aimed particularly at dealing efficiently with network protocols and complicated text/binary file formats".

Attoparsec offers not only speed and efficiency, but backtracking and incremental input. 

Its API closely mirrors that of another parser combinator library, Parsec.

There are submodules for compatibility with `ByteString`, `Text` and `Char8`. Use of the `OverloadedStrings` language extension is recommended.

## Parameters
| Type | Detail |
|------|--------|
| `Parser i a` | The core type for representing a parser. `i` is the string type, e.g. `ByteString`. |
| [`IResult i r`](https://hackage.haskell.org/package/attoparsec-0.13.1.0/docs/Data-Attoparsec-Internal-Types.html#t:IResult) | The result of a parse, with `Fail i [String] String`, `Partial (i -> IResult i r)` and `Done i r` as constructors. |

## Combinators
Parsing input is best achieved through larger parser functions that are composed of smaller, single purpose ones.

Let's say we wished to parse the following text which represents working hours:

> Monday: 0800 1600.

We could split these into two "tokens": the day name -- "Monday" -- and a time portion "0800" to "1600".

To parse a day name, we could write the following:

```haskell
data Day = Day String

day :: Parser Day
day = do
  name <- takeWhile1 (/= ':')
  skipMany1 (char ':')
  skipSpace
  return $ Day name
```

To parse the time portion we could write:

```haskell
data TimePortion = TimePortion String String

time = do
    start <- takeWhile1 isDigit
    skipSpace
    end <- takeWhile1 isDigit
    return $ TimePortion start end
```

Now we have two parsers for our individual parts of the text, we can combine these in a "larger" parser to read an entire day's working hours:

```haskell
data WorkPeriod = WorkPeriod Day TimePortion

work = do
    d <- day
    t <- time
    return $ WorkPeriod d t
    
```

and then run the parser:

```haskell
parseOnly work "Monday: 0800 1600"
```




## Bitmap - Parsing Binary Data
Attoparsec makes parsing binary data trivial. Assuming these definitions:

```haskell
import           Data.Attoparsec.ByteString (Parser, eitherResult, parse, take)
import           Data.Binary.Get            (getWord32le, runGet)
import           Data.ByteString            (ByteString, readFile)
import           Data.ByteString.Char8      (unpack)
import           Data.ByteString.Lazy       (fromStrict)
import           Prelude                    hiding (readFile, take)

-- The DIB section from a bitmap header
data DIB = BM | BA | CI | CP | IC | PT
           deriving (Show, Read)

type Reserved = ByteString

-- The entire bitmap header
data Header = Header DIB Int Reserved Reserved Int
              deriving (Show)
```

We can parse the header from a bitmap file easily. Here, we have 4 parser functions that represent the header section from a bitmap file:

Firstly, the DIB section can be read by taking the first 2 bytes

```
dibP :: Parser DIB
dibP = read . unpack <$> take 2
```

Similarly, the size of the bitmap, the reserved sections and the pixel offset can be read easily too:

```haskell
sizeP :: Parser Int
sizeP = fromIntegral . runGet getWord32le . fromStrict <$> take 4

reservedP :: Parser Reserved
reservedP = take 2

addressP :: Parser Int
addressP = fromIntegral . runGet getWord32le . fromStrict <$> take 4
```

which can then be combined into a larger parser function for the entire header:

```haskell
bitmapHeader :: Parser Header
bitmapHeader = do
    dib <- dibP
    sz <- sizeP
    reservedP
    reservedP
    offset <- addressP
    return $ Header dib sz "" "" offset
```


