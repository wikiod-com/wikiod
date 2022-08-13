---
title: "Advanced usage"
slug: "advanced-usage"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

Please note that the Twig extension is [not compatible with PHP7][1] and although there is a [pull request][2] to resolve this situation it was not yet made part of version 1.x. With the most recent Twig 2.x release the C extension [was removed][3] but [might be part of 2.1][4]. Some benchmarks reveal [minimal improvements][5] with the C extension under PHP7.


  [1]: https://github.com/twigphp/Twig/issues/2012#issuecomment-204682406
  [2]: https://github.com/twigphp/Twig/pull/2310
  [3]: https://github.com/twigphp/Twig/blob/2.x/CHANGELOG
  [4]: https://github.com/twigphp/Twig/pull/2310#issuecomment-269231405
  [5]: https://github.com/twigphp/Twig/pull/2310#issuecomment-269231224

## Building the C Extension
The C extension is an optional feature of Twig that offers some performance improvements of template rendering. The source code for the extension is located in the Twig source code directory at `ext/twig`. It compiles like any other PHP extentension:

    cd ext/twig
    phpize
    ./configure
    make
    make install

Enable the extension in the `php.ini` file. Once the extension is installed, Twig will automatically detect and use it at runtime.

