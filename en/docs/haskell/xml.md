---
title: "XML"
slug: "xml"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

Encoding and decoding of XML documents.

## Encoding a record using the `xml` library
    {-# LANGUAGE RecordWildCards #-}
    import Text.XML.Light

    data Package = Package
      { pOrderNo  :: String
      , pOrderPos :: String
      , pBarcode  :: String
      , pNumber   :: String
      }

    -- | Create XML from a Package
    instance Node Package where
      node qn Package {..} =
        node qn
          [ unode "package_number" pNumber
          , unode "package_barcode" pBarcode
          , unode "order_number" pOrderNo
          , unode "order_position" pOrderPos
          ]

