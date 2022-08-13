---
title: "Base64 Encoding"
slug: "base64-encoding"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## Syntax
- func (enc *base64.Encoding) Encode(dst, src []byte)
- func (enc *base64.Encoding) Decode(dst, src []byte) (n int, err error)
- func (enc *base64.Encoding) EncodeToString(src []byte) string
- func (enc *base64.Encoding) DecodeString(s string) ([]byte, error)

The [`encoding/base64`](https://godoc.org/encoding/base64) package contains several [built in encoders](https://godoc.org/encoding/base64#pkg-variables). Most of the examples in this document will use `base64.StdEncoding`, but any encoder (`URLEncoding`, `RawStdEncodign`, your own custom encoder, etc.) may be substituted.

## Encoding
<!-- language: lang-go -->

    const foobar = `foo bar`
    encoding := base64.StdEncoding
    encodedFooBar := make([]byte, encoding.EncodedLen(len(foobar)))
    encoding.Encode(encodedFooBar, []byte(foobar))
    fmt.Printf("%s", encodedFooBar)
    // Output: Zm9vIGJhcg==

[Playground](https://play.golang.org/p/A5c_BSMFrQ)

## Encoding to a String
<!-- language: lang-go -->

    str := base64.StdEncoding.EncodeToString([]byte(`foo bar`))
    fmt.Println(str)
    // Output: Zm9vIGJhcg==

[Playground](https://play.golang.org/p/vpbKRkEtsU)

## Decoding
<!-- language: lang-go -->

    encoding := base64.StdEncoding
    data := []byte(`Zm9vIGJhcg==`)
    decoded := make([]byte, encoding.DecodedLen(len(data)))
    n, err := encoding.Decode(decoded, data)
    if err != nil {
        log.Fatal(err)
    }

    // Because we don't know the length of the data that is encoded
    // (only the max length), we need to trim the buffer to whatever
    // the actual length of the decoded data was.
    decoded = decoded[:n]

    fmt.Printf("`%s`", decoded)
    // Output: `foo bar`

[Playground](https://play.golang.org/p/J5qxlJpaCL)

## Decoding a String
<!-- language: lang-go -->

    decoded, err := base64.StdEncoding.DecodeString(`biws`)
    if err != nil {
        log.Fatal(err)
    }
    
    fmt.Printf("%s", decoded)
    // Output: n,,

[Playground](https://play.golang.org/p/h2qngYncRs)

