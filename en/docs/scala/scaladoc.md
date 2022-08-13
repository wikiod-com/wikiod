---
title: "Scaladoc"
slug: "scaladoc"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

## Syntax
 - Goes above methods, fields, classes or packages.
 - Starts with `/**`
 - Each line has an starting `*` proceding with the comments
 - Ends with `*/`

## Parameters
| Parameter                                 | Details                                                                                              |
|-------------------------------------------|------------------------------------------------------------------------------------------------------|
| __Class specific__                        | _                                                                                                    |
| `@constructor detail`                     | Explains the main constructor of the class                                                           |
| __Method specific__                       | _                                                                                                    |
| `@return detail`                          | Details about what is returned on the method.                                                        |
| __Method, Constructor and/or Class tags__ | _                                                                                                    |
| `@param x detail`                         | Details about the value parameter `x` on a method or constructor.                                    |
| `@tparam x detail`                        | Details about the type parameter `x` on a method or constructor.                                     |
| `@throws detail`                          | What exceptions may be thrown.                                                                       |
| __Usage__                                 | _                                                                                                    |
| `@see detail`                             | References other sources of information.                                                             |
| `@note detail`                            | Adds a note for pre or post conditions, or any other notable restrictions or expectations.           |
| `@example detail`                         | Provides example code or related example documentation.                                              |
| `@usecase detail`                         | Provides a simplified method definition for when the full method definition is too complex or noisy. |
| __Other__                                 | _                                                                                                    |
| `@author detail`                          | Provides information about the author of the following.                                              |
| `@version detail`                         | Provides the version that this portion belongs to.                                                   |
| `@deprecated detail`                      | Marks the following entity as deprecated.                                                            |


## Simple Scaladoc to method
    /**
      * Explain briefly what method does here
      * @param x Explain briefly what should be x and how this affects the method.
      * @param y Explain briefly what should be y and how this affects the method.
      * @return Explain what is returned from execution.
      */
    def method(x: Int, y: String): Option[Double] = {
      // Method content
    }

