---
title: "Packages"
slug: "packages"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

Packages in Scala manage namespaces in large programs. For example, the name `connection` can occur in the packages `com.sql` and `org.http`. You can use the fully qualified `com.sql.connection` and `org.http.connection`, respectively, in order to access each of these packages.

## Package structure
    package com {
        package utility {
            package serialization {
                class Serializer
                ...        
            }     
        }
    }





## Packages and files
The package clause is not directly binded with the file where it is found. It is possible to find common elements of the package clause in diferent files. For example, the package clauses bellow can be found in the file math1.scala and in the file math2.scala.

> File math1.scala

    package org {
        package math {
            package statistics {
                class Interval
            }    
        }
    }

> File math2.scala

    package org {
        package math{
            package probability {
                class Density
            }
        }
    }


> File study.scala

    import org.math.probability.Density
    import org.math.statistics.Interval

    object Study {

        def main(args: Array[String]): Unit = {
            var a = new Interval()
            var b = new Density()
        }
    }






## Package naming convension
Scala packages should follow the Java package naming conventions.  
Package names are written in all lower case to avoid conflict with the names of classes or interfaces.
Companies use their reversed Internet domain name to begin their package names—for example,

    io.super.math 

