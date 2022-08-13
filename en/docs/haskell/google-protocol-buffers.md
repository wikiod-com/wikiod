---
title: "Google Protocol Buffers"
slug: "google-protocol-buffers"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

To use Protocol Buffers with Haskell you should install the `htprotoc` package:

 1. Clone the project from [Github][1]
 2. Use [Stack][2] to build and install

You should now find the `hprotoc` executable in `$HOME/.local/bin/`.

  [1]: https://github.com/k-bx/protocol-buffers
  [2]: https://www.wikiod.com/haskell/stack

## Creating, building and using a simple .proto file
Let us first create a simple `.proto` file `person.proto`

    package Protocol;
    
    message Person {
        required string firstName = 1;
        required string lastName  = 2;
        optional int32  age       = 3;
    }

After saving we can now create the Haskell files which we can use in our project by running

    $HOME/.local/bin/hprotoc --proto_path=. --haskell_out=. person.proto

We should get an output similar to this:
    
    Loading filepath: "/<path-to-project>/person.proto"
    All proto files loaded
    Haskell name mangling done
    Recursive modules resolved
    ./Protocol/Person.hs
    ./Protocol.hs
    Processing complete, have a nice day.

`hprotoc` will create a new folder `Protocol` in the current directory with `Person.hs` which we can simply import into our haskell project:
    
    import Protocol (Person)

As a next step, if using [Stack][1] add 
    
       protocol-buffers
     , protocol-buffers-descriptor

to `build-depends:` and 

    Protocol
    
to `exposed-modules` in your `.cabal` file. 

If we get now a incoming message from a stream, the message will have the type `ByteString`.

In order to transform the `ByteString` (which obviously should contain encoded "Person" data) into our Haskell data type, we need to call the function `messageGet` which we import by

    import Text.ProtocolBuffers (messageGet)

which enables to create a value of type `Person` using:

    transformRawPerson :: ByteString -> Maybe Person
    transformRawPerson raw = case messageGet raw of
        Left   _           -> Nothing
        Right (person, _)  -> Just person


  [1]: https://www.wikiod.com/haskell/stack

