---
title: "Exceptions"
slug: "exceptions"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

Dart code can throw and catch exceptions. Exceptions are errors indicating that something unexpected happened. If the exception isn’t caught, the isolate that raised the exception is suspended, and typically the isolate and its program are terminated.

In contrast to Java, all of Dart’s exceptions are unchecked exceptions. Methods do not declare which exceptions they might throw, and you are not required to catch any exceptions.

Dart provides [Exception][1] and [Error][2] types, as well as numerous predefined subtypes. You can, of course, define your own exceptions. However, Dart programs can throw any non-null object—not just Exception and Error objects—as an exception.


  [1]: https://api.dartlang.org/stable/dart-core/Exception-class.html
  [2]: https://api.dartlang.org/stable/dart-core/Error-class.html

## Custom exception
    class CustomException implements Exception {
      String cause;
      CustomException(this.cause);
    }
    
    void main() {
      try {
        throwException();
      } on CustomException {
        print("custom exception is been obtained");
      }
    }
    
    throwException() {
      throw new CustomException('This is my first custom exception');
    }

