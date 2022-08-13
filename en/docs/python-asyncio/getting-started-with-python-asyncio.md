---
title: "Getting started with python-asyncio"
slug: "getting-started-with-python-asyncio"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
In order to install asyncio:

    pip install asyncio

Notice that python asyncio requires Python 3.3 or later.

This module became part of the Python standard library since Python 3.4.

## Simple Example- Printing 'Hello World' with asyncio
    import asyncio

    async def hello_world():
        print('Hello World')

    loop = asyncio.get_event_loop()
    loop.run_until_complete(hello_world())
    loop.close()

