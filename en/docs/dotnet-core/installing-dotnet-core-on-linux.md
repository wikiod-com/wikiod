---
title: "Installing .NET Core on Linux"
slug: "installing-net-core-on-linux"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

## Generic installation for Linux distributions
If you have one of the supported Linux distributions, you can follow the steps on the .NET Core website: https://www.microsoft.com/net

If you have an unsupported distribution:

Download the **.NET Core SDK** from the links, picking the distribution closer to the used one.

https://www.microsoft.com/net/download

If you have support for **deb** packages, you can install **Ubuntu/Debian** packages.

If you have support for **yum** packages, you can install **Fedora** packages.

Make sure your system has at least:

    llvm-3.7.1-r3
    libunwind-1.1-r1
    icu-57.1
    lttng-ust-2.8.1
    openssl-1.0.2h-r2
    curl-7.49.0

