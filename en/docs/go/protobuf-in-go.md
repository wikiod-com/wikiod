---
title: "Protobuf in Go"
slug: "protobuf-in-go"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

**Protobuf** or Protocol Buffer encodes and decodes data so that different applications or modules written in unlike languages can exchange the large number of messages quickly and reliably without overloading the communication channel. With protobuf, the performance is directly proportional to the number of message you tend to send. It compress the message to send in a serialized binary format by providing your the tools to encode the message at source and decode it at the destination.

There are two steps of using **protobuf**. 

 1. First you must compile the protocol buffer definitions
 2. Import the above definitions, with the support library into your program.

**gRPC Support**

If a proto file specifies RPC services, protoc-gen-go can be instructed to generate code compatible with gRPC (http://www.grpc.io/). To do this, pass the `plugins` parameter to protoc-gen-go; the usual way is to insert it into the --go_out argument to protoc:

    protoc --go_out=plugins=grpc:. *.proto




## Using Protobuf with Go
The message you want to serialize and send that you can include into a file **test.proto**, containing

    package example;
    
    enum FOO { X = 17; };
    
    message Test {
      required string label = 1;
      optional int32 type = 2 [default=77];
      repeated int64 reps = 3;
      optional group OptionalGroup = 4 {
        required string RequiredField = 5;
      }
    }

To compile the protocol buffer definition, run protoc with the --go_out parameter set to the directory you want to output the Go code to.

    protoc --go_out=. *.proto

To create and play with a Test object from the example package,

    package main

    import (
        "log"

        "github.com/golang/protobuf/proto"
        "path/to/example"
    )

    func main() {
        test := &example.Test {
            Label: proto.String("hello"),
            Type:  proto.Int32(17),
            Reps:  []int64{1, 2, 3},
            Optionalgroup: &example.Test_OptionalGroup {
                RequiredField: proto.String("good bye"),
            },
        }
        data, err := proto.Marshal(test)
        if err != nil {
            log.Fatal("marshaling error: ", err)
        }
        newTest := &example.Test{}
        err = proto.Unmarshal(data, newTest)
        if err != nil {
            log.Fatal("unmarshaling error: ", err)
        }
        // Now test and newTest contain the same data.
        if test.GetLabel() != newTest.GetLabel() {
            log.Fatalf("data mismatch %q != %q", test.GetLabel(), newTest.GetLabel())
        }
        // etc.
    }

To pass extra parameters to the plugin, use a comma-separated parameter list separated from the output directory by a colon:

    protoc --go_out=plugins=grpc,import_path=mypackage:. *.proto


