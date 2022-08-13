---
title: "IO"
slug: "io"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

## Handling existing InputStreams and OutputStreams
Reading the content of an `InputStream` as a `byte` array:

    // Reading from a file
    try (InputStream in = new FileInputStream("in.dat")) {
      byte[] content = ByteStreams.toByteArray(in);
      // do something with content
    }

Copying an `InputStream` to an `OutputStream`:

    // Copying the content from a file in.dat to out.dat.
    try (InputStream in = new FileInputStream("in.dat");
         OutputStream out = new FileOutputStream("out.dat")) {
      ByteStreams.copy(in, out);
    }

Note: to copy files directly, it's better to use `Files.copy(sourceFile, destinationFile)`.

Reading an entire predefined `byte` array from an `InputStream`:

    try (InputStream in = new FileInputStream("in.dat")) {
      byte[] bytes = new byte[16];
      ByteStreams.readFully(in, bytes);
      // bytes is totally filled with 16 bytes from the InputStream.
    } catch (EOFException ex) {
      // there was less than 16 bytes in the InputStream.
    }

Skipping `n` bytes from the `InputStream`:

    try (InputStream in = new FileInputStream("in.dat")) {
      ByteStreams.skipFully(in, 20);
      // the next byte read will be the 21st.
      int data = in.read();
    } catch (EOFException e) {
      // There was less than 20 bytes in the InputStream.
    }

Creating an `OutputStream` that discards everything that is written to it:

    try (InputStream in = new FileInputStream("in.dat");
         OutputStream out = ByteStreams.nullOutputStream()) {
      ByteStreams.copy(in, out);
      // The whole content of in is read into... nothing.
    }

## Handling existing Readers and Writers
Reading the content of a `Reader` as a `String`:

    // Reading from a file
    try (Reader reader = new FileReader("in.txt")) {
      String content = CharStreams.toString(reader);
      // do something with content
    }

Reading the content of a `Reader` as a list of line contents:

    try (Reader reader = new FileReader("in.txt")) {
      List<String> lines = CharStreams.readLines(reader);
      for (String line: lines) {
        // Do something with line
      }
    }

Copying a `Reader` to a `Writer`:

    try (Reader reader = new FileReader("in.txt");
         Writer writer = new FileWriter("out.txt")) {
      CharStreams.copy(reader, writer);
    }

Note: to copy files directly, it's better to use Files.copy(sourceFile, destinationFile).

Skipping `n` bytes from the `Reader`:

    try (Reader reader = new FileReader("in.txt")) {
      CharStreams.skipFully(reader, 20);
      // The next char read will be the 21st.
    } catch (EOFException e) {
      // There was less than 20 chars in the Reader.
    }

Creating a `Writer` that discards everything that is written to it:

    try (Reader reader = new FileReader("in.txt");
         Writer writer = CharStreams.nullWriter()) {
      CharStreams.copy(reader, writer);
      // The whole content of reader is read into... nothing.
    }

## Sources and sinks
Sources and sinks are objects that know how to open streams.

|             | Bytes        | Chars        | 
| ----------- | ------------ | ------------ |
| **Reading** | `ByteSource` | `CharSource` |
| **Writing** | `ByteSink`   | `CharSink`   |

# Creating sources and sinks

Note: for all examples, consider `UTF_8` as if the following import is set:

    import static java.nio.charset.StandardCharsets.UTF_8;

## Reading from a file

    ByteSource dataSource = Files.asByteSource(new File("input.dat"));
    CharSource textSource = Files.asCharSource(new File("input.txt"), UTF_8);

## Writing to a file

    ByteSink dataSink = Files.asByteSink(new File("output.dat"));
    CharSink textSink = Files.asCharSink(new File("output.txt"), UTF_8);

## Reading from a URL

    ByteSource dataSource = Resources.asByteSource(url);
    CharSource textSource = Resources.asCharSource(url, UTF_8);

## Reading from in memory data

    ByteSource dataSource = ByteSource.wrap(new byte[] {1, 2, 3});
    CharSource textSource = CharSource.wrap("abc");

## Converting from bytes to chars

    ByteSource originalSource = ...
    CharSource textSource = originalSource.asCharSource(UTF_8);

## Converting from chars to bytes

(From Guava 20 onwards)

    CharSource originalSource = ...
    ByteSource dataSource = originalSource.asByteSource(UTF_8);

# Using sources and sinks

## Common operations

Opening a stream

    InputStream inputStream = byteSource.openStream();
    OutputStream outputStream = byteSink.openStream();
    Reader reader = charSource.openStream();
    Writer writer = charSink.openStream();

Opening a buffered stream

    InputStream bufferedInputStream = byteSource.openBufferedStream();
    OutputStream bufferedOutputStream = byteSink.openBufferedStream();
    BufferedReader bufferedReader = charSource.openBufferedStream();
    Writer bufferedWriter = charSink.openBufferedStream();

## Source operations

Reading from a source:

    ByteSource source = ...
    byte[] bytes = source.read();

    CharSource source = ...
    String text = source.read();

Reading lines from a source:

    CharSource source = ...
    ImmutableList<String> lines = source.readLines();

Reading the first line from a source:

    CharSource source = ...
    String firstLine = source.readFirstLine();

Copying from a source to a sink:

    ByteSource source = ...
    ByteSink sink = ...
    source.copyTo(sink);

    CharSource source = ...
    CharSink sink = ...
    source.copyTo(sink);

## Typical usage

    CharSource source = ...
    try (Reader reader = source.openStream()) {
      // use the reader
    }

