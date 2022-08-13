---
title: "logging in kotlin"
slug: "logging-in-kotlin"
draft: false
images: []
weight: 9954
type: docs
toc: true
---

Related question: http://stackoverflow.com/q/34416869/986533

## kotlin.logging
    class FooWithLogging {
      companion object: KLogging()

      fun bar() {
        logger.info { "hello $name" }
      }

      fun logException(e: Exception) {
        logger.error(e) { "Error occured" }
      }
    }


Using [kotlin.logging][1] framework


  [1]: https://github.com/MicroUtils/kotlin.logging

